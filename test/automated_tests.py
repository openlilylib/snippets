#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys
import re

from common_functions import print_separator, home_dir, install_root


class SimpleTests:
    """Run simple intergration tests. Specifically, this script will look
    for all the files in `usage-examples` directories. All these files
    will be compiled with LilyPond. If the compilation results in a
    non-zero exit code, then that test is marked as failed.

    If a file named `.simple-tests-exclude` is found in a
    `usage-example` directory, then all the files listed in that file
    are excluded from the test set.

    This class can be run in two different modes:

      - Continuous integration: this mode is active when the tests are
        run by a continuous integration server. It is assumed that the
        appropriate LilyPond version is already present, either through
        the cache or downloaded by the 'before_script' handler.

      - Local mode: used on local computers.
        If a command string is passed to the constructor this is used
        as the LilyPond call, otherwise this defaults to "lilypond"

    Used environment variables

      - CI : if 'true' assume that we are running in continuous
        integration environment.

      - LILY_VERSION : the version we want to test, like '2.19.15-1'

    """

    # configuration of variable names
    ci_env_var = "CI"
    lily_version_var = "LILY_VERSION"

    test_excludes_fname = ".automated-tests-exclude"
    test_includes_fname = ".automated-tests-include"
    examples_dirname = "usage-examples"

    def __init__(self, cmd=None):
        # set up building environment

        # root directory
        self.openlilylib_dir = self.__openlilylib_dir()

        # LilyPond command
        if self.is_ci_run():
            try:
                self.lilypond_version = os.environ[self.lily_version_var]
                self.lily_command = osp.join(install_root,
                                             "bin",
                                             "lilypond")
            except KeyError:
                sys.exit('Environment variable {} not set. Aborting'.format(self.lily_version_var))
        else:
            self.lily_command = cmd if cmd else "lilypond"
            self.lilypond_version = self.__lilypond_version()

        # Add include path and other options to generated LilyPond command
        self.lily_command_with_includes = [self.lily_command,
                "-dno-point-and-click",
                "-I", self.openlilylib_dir,
                "-I", os.path.join(self.openlilylib_dir, "ly")]
        # initialize some lists
        self.test_files = []
        self.included_tests = []
        self.excluded_tests = []
        self.failed_tests = {}


# private functions
    def __collect_all_in_dir(self, dirname):
        """Read contents of a directory and collect test files.
           Respect include and exclude files and get all files
           from usage-examples directories, recursively."""

        # process include/exclude files if present
        includes_fname = osp.join(dirname, self.test_includes_fname)
        self.included_tests.extend(self.__read_include_exclude_file(includes_fname))

        excludes_fname = osp.join(dirname, self.test_excludes_fname)
        self.excluded_tests.extend(self.__read_include_exclude_file(excludes_fname))

        # add LilyPond files if we're in a usage-examples directory, recursively
        if osp.basename(dirname) == self.examples_dirname:
            for root, _, files in os.walk(dirname):
                for f in files:
                    test_fname = osp.join(root, f)
                    print test_fname
                    if os.path.isfile(test_fname) and self.is_lilypond_file(test_fname):
                        self.test_files.append(test_fname)


    def __lilypond_version(self):
        """Determine the LilyPond version actually run by the command self.lily_command"""
        lily = sp.Popen([self.lily_command, "-v"], stdout=sp.PIPE, stderr=sp.PIPE)
        version_line = lily.communicate()[0].splitlines()[0]
        return re.search(r"\d+\.\d+\.\d+", version_line).group(0)


    def __openlilylib_dir(self):
        """Return the root directory of openLilyLib.
           It's the parent directory of the script."""
        script_path = osp.abspath(osp.dirname(osp.realpath(__file__)))
        return osp.abspath(osp.join(script_path, os.pardir))


    def __read_include_exclude_file(self, fname):
        """Parse a include or exclude file and return
           a list with absolute paths, joining the
           directory of the include file with the
           relative file names included in it."""
        result = []
        if osp.exists(fname):
            with open(fname, 'r') as lines:
                for line in lines.readlines():
                    line = line.strip()
                    if not line.startswith("#") and len(line) > 0:
                        result.append(osp.abspath(
                            osp.join(osp.dirname(fname), line)))
        return result


    def __relative_path(self, fname):
        """Return the filename relative to openlilylib_dir"""
        return fname[len(self.openlilylib_dir) + 1:]


# public functions
    def clean_results_dir(self):
        """Remove any existing results directory,
           taking LilyPond version into account."""
        print "\nClear previous test results (if any)."
        results_dir = os.path.join(self.openlilylib_dir,
                                   "test",
                                   "results",
                                   self.lilypond_version)
        if os.path.exists(results_dir):
            print "Removing result dir:",results_dir
            shutil.rmtree(results_dir)


    def collect_tests(self):
        """Iterate over the whole openLilyLib directory and
           collect valid test files, either all LilyPond files in
           'usage-examples' directories or files specified in
           .simple-tests-include files anywhere.
           .simple-tests-excludes are used to skip files."""

        print_separator()
        print "Collecting test files\n"

        # iterate over directory structure
        for root, _, files in os.walk(self.openlilylib_dir):
            self.__collect_all_in_dir(root)

        # build definitive list of test cases:
        self.test_files = [t for t in self.test_files if t not in self.excluded_tests]
        self.test_files.extend([t for t in self.included_tests if t not in self.excluded_tests])
        self.test_files.sort()

        # print summary about test cases
        print "Found {} test files:".format(len(self.test_files))
        print "\n".join([self.__relative_path(t) for t in self.test_files])
        print "\n"

        print "Potential test files explicitly excluded:"
        print "\n".join([self.__relative_path(t) for t in self.excluded_tests])


    def is_ci_run(self):
        """True if tests are running in continuous integration environment"""
        return self.ci_env_var in os.environ and os.environ[self.ci_env_var] == "true"


    def is_lilypond_file(self, fname):
        """Return true if filename ends with one of the registered file extensions."""
        return fname.endswith('.ly') or fname.endswith('.ily')


    def is_runnable_file(self, fname):
        """Returns true if fname can be compiled with the lilypond version used"""
        if not self.is_lilypond_file(fname):
            return False
        with open(fname, 'r') as fcontents:
            for line in fcontents.readlines():
                version_line = re.search(r"\\version \"(\d+\.\d+\.\d+)\"", line)
                if version_line:
                    file_version = version_line.group(1).split(".")
                    lily_version = self.lilypond_version.split(".")
                    if file_version == lily_version:
                        return True
                    for (fv, lv) in zip(file_version, lily_version):
                        if int(fv) < int(lv):
                            return True
                    print "**WARNING** File version " +\
                        version_line.group(1) +\
                        " greater than lilypond version {}, skipping".format(
                            self.lilypond_version)
                    return False
        print "**WARNING** No version line found, skipping", fname
        return False


    def print_introduction(self):
        """Print some useful output about the testing environment"""

        print "\n================================"            # if test failed, add it to the list of failed tests to be reported later

        print "openLilyLib automated test suite"
        print "================================\n"

        print "Running LilyPond", self.lilypond_version
        print "OpenLilyLib directory: {}".format(self.openlilylib_dir)

        print "LilyPond command to be used:"
        print " ".join(self.lily_command_with_includes + ["-o <output-dir> <test-file>"])


    def report(self):
        """Give a summary about testing results and
           print detailed LilyPond log for any failed tests."""

        print "Summary:\n"
        print "  {} failed tests out of {}".format(
            len(self.failed_tests), len(self.test_files)), "\n"

        if len(self.failed_tests) > 0:
            fail_list = [t for t in self.failed_tests]
            fail_list.sort()
            print "Failed tests:"
            for test in fail_list:
                print "-", self.__relative_path(test)

            print "\nDetails for failed tests:\n"
            for test in fail_list:
                test_name = self.__relative_path(test)
                print test_name
                print "-" * len(test_name)
                print self.failed_tests[test]
                print ""
            print_separator()
            sys.exit(1)


    def run(self):
        """Run the tests collected earlier"""
        print_separator()
        print "Running tests now\n"

        for test in self.test_files:
            print "Running test", self.__relative_path(test)
            test_result_dir = os.path.join(self.openlilylib_dir,
                                    "test",
                                    "results",
                                    self.lilypond_version,
                                    os.path.dirname(self.__relative_path(test)))
            if not os.path.exists(test_result_dir):
                os.makedirs(test_result_dir)
            lily = sp.Popen(self.lily_command_with_includes + ['-o',
                                                               test_result_dir,
                                                               test],
                            stdout=sp.PIPE, stderr=sp.PIPE)
            (out, err) = lily.communicate()

            if lily.returncode == 0:
                print "------- OK! --------"
            else:
                # if test failed, add it to the list of failed tests to be reported later
                self.failed_tests[test] = err
                print "\n====== FAILED ======"
                print "See details at the end of test run."
        print_separator()

# main program flow
if __name__ == "__main__":

    # instantiate Tests object
    if len(sys.argv) > 1:
        tests = SimpleTests(sys.argv[1])
    else:
        tests = SimpleTests()

    # go through the procedure step by step
    tests.print_introduction()
    tests.clean_results_dir()
    tests.collect_tests()
    tests.run()
    tests.report()
