#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys
import re

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
        run by a continuous integration server. LilyPond will be
        downloaded from the internet.

      - Local mode: used on local computers, the lilypond executable
        needs to be passed to the constructor.

    Used environment variables

      - CI : if 'true' assume that we are running in continuous
        integration environment.

      - LILY_PLATFORM : the lilypond platform we are running on, for
        instance 'linux-64' for 64 bit linux

      - LILY_VERSION : the version we want to test, like '2.19.15-1'

    """

    ci_env_var = "CI"
    lily_platform_var = "LILY_PLATFORM"
    lily_version_var = "LILY_VERSION"

    tmp_lily_dir = "/tmp/lilypond"

    lily_install_script = "/tmp/lilypond-install.sh"

    binary_site = "http://download.linuxaudio.org/lilypond/binaries/"

    test_list_fname = ".simple-tests"
    test_excludes_fname = ".simple-tests-exclude"
    test_includes_fname = ".simple-tests-include"
    examples_dirname = "usage-examples"

    def __init__(self, cmd=None):
        self.clean_tmp_dir()
        if self.is_ci_run():
            self.__install_distribution()
            self.lily_command = osp.join(self.tmp_lily_dir, "bin/lilypond")
        elif not cmd == None:
            self.lily_command = cmd
        else:
            self.lily_command = "lilypond"

        self.lilypond_version = self.__lilypond_version()
        self.openlilylib_dir = self.__openlilylib_dir()
        self.relative_path_start_index = len(self.openlilylib_dir) + 1

        self.lily_command_with_includes = [self.lily_command,
                   "-I", self.openlilylib_dir,
                   "-I", os.path.join(self.openlilylib_dir, "ly")]

        self.test_files = []
        self.included_tests = []
        self.excluded_tests = []


    def clean_results_dir(self):
        results_dir = os.path.join(self.openlilylib_dir,
                                   "test",
                                   "results",
                                   self.lilypond_version)
        if os.path.exists(results_dir):
            print "Removing result dir:",results_dir
            shutil.rmtree(results_dir)

    def clean_tmp_dir(self):
        if os.path.exists(self.tmp_lily_dir):
            shutil.rmtree(self.tmp_lily_dir)
        if os.path.exists(self.lily_install_script):
            os.remove(self.lily_install_script)

    def is_ci_run(self):
        """True if tests are running in continuous integration environment"""
        return self.ci_env_var in os.environ and os.environ[self.ci_env_var] == "true"

    def __download_url(self):
        if not (self.lily_platform_var in os.environ
                and self.lily_version_var in os.environ):
            raise KeyError("{} and {} must be set".format(
                self.lily_platform_var, self.lily_version_var))
        platform = os.environ[self.lily_platform_var]
        version = os.environ[self.lily_version_var]
        return "{}/{}/lilypond-{}.{}.sh".format(
            self.binary_site, platform, version, platform)

    def __install_distribution(self):
        sp.check_call(
            ["wget", "-O", self.lily_install_script, self.__download_url()])
        sp.check_call(["sh", self.lily_install_script,
                       "--prefix", self.tmp_lily_dir,
                       "--batch"])

    def __lilypond_version(self):
        lily = sp.Popen([self.lily_command, "-v"], stdout=sp.PIPE, stderr=sp.PIPE)
        version_line = lily.communicate()[0].splitlines()[0]
        return re.search(r"\d+\.\d+\.\d+", version_line).group(0)

    def is_runnable_file(self, fname):
        """Returns true if fname can be compiled with the lilypond version used"""
        if not (fname.endswith(".ly") or fname.endswith(".ily")):
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

    def __openlilylib_dir(self):
        script_path = osp.abspath(osp.dirname(osp.realpath(__file__)))
        return osp.abspath(osp.join(script_path, os.pardir))


    def __relative_path(self, fname):
        """Return the filename relative to openlilylib_dir"""
        return fname[self.relative_path_start_index:]

    def __collect_all_in_dir(self, dirname):
        test_files = []

        includes_fname = osp.join(dirname, self.test_includes_fname)
        if osp.exists(includes_fname):
            print "Found includes file:\n", self.__relative_path(includes_fname)
            with open(includes_fname, 'r') as includes_lines:
                inc_cnt = 0
                for line in includes_lines.readlines():
                    included_fname = line.strip()
                    if not line.startswith("#") and len(included_fname) > 0:
                        inc_cnt += 1
                        to_include = osp.abspath(
                            osp.join(dirname, included_fname))
                        self.included_tests.append(to_include)
                print "Including {} files.".format(inc_cnt)

        excludes_fname = osp.join(dirname, self.test_excludes_fname)
        if osp.exists(excludes_fname):
            print "Found excludes file:\n", self.__relative_path(excludes_fname)
            with open(excludes_fname, 'r') as excludes_lines:
                exc_cnt = 0
                for line in excludes_lines.readlines():
                    excluded_fname = line.strip()
                    if not line.startswith("#") and len(excluded_fname) > 0:
                        exc_cnt += 1
                        to_exclude = osp.abspath(
                            osp.join(dirname, excluded_fname))
                        if not to_exclude in self.excluded_tests:
                            self.excluded_tests.append(to_exclude)
                print "Excluding {} files.".format(exc_cnt)

        if osp.basename(dirname) == self.examples_dirname:
            print "\nEntering examples directory:\n", self.__relative_path(dirname)
            inc_cnt = 0
            for root, _, files in os.walk(dirname):
                for f in files:
                    test_fname = osp.abspath(osp.join(root, f))
                    if self.is_runnable_file(test_fname) \
                            and not test_fname in self.excluded_tests:
                        inc_cnt += 1
                        test_files.append(test_fname)
            print "Found {} files.".format(inc_cnt)

        return test_files



    def __collect_tests(self):

        print_separator()
        print "Collecting test files\n"
        test_files = []
        for root, _, files in os.walk(self.openlilylib_dir):
            test_files.extend(self.__collect_all_in_dir(root))

        # check again for excluded files
        # (as the exclude file may be in another directory)
        for i in self.included_tests:
            if not i in self.excluded_tests:
                self.test_files.append(i)
        for t in test_files:
            if not t in self.excluded_tests:
                self.test_files.append(t)


    def run(self):
        self.__collect_tests()

        print_separator()
        print "Found the following test files:"
        print "\n".join([self.__relative_path(t) for t in self.test_files])
        print "\n"

        print "Potential test files explicitly excluded:"
        print "\n".join([self.__relative_path(t) for t in self.excluded_tests])

        print_separator()
        print "Running tests now\n"

        failed_tests = {}
        for test in self.test_files:
            print "\n\nRunning test", self.__relative_path(test)
            test_dir = os.path.join(self.openlilylib_dir,
                                    "test",
                                    "results",
                                    self.lilypond_version,
                                    os.path.dirname(self.__relative_path(test)))
            if not os.path.exists(test_dir):
                os.makedirs(test_dir)
            lily = sp.Popen(self.lily_command_with_includes + ['-o',
                                                               test_dir,
                                                               test],
                            stdout=sp.PIPE, stderr=sp.PIPE)
            (out, err) = lily.communicate()
            if lily.returncode != 0:
                failed_tests[test] = err
                print "\n====== FAILED ======"
                print "See details at the end of test run."
            else:
                print "------- OK! --------"
        print_separator()
        print "  {} failed tests out of {}".format(
            len(failed_tests), len(self.test_files)), "\n"
        print_separator()
        if len(failed_tests) > 0:
            fail_list = [t for t in failed_tests]
            fail_list.sort()
            print "Failed tests:"
            for test in fail_list:
                print " ", self.__relative_path(test)

            print "\nDetails for failed tests:\n"
            for test in fail_list:
                print " ", self.__relative_path(test)
                print failed_tests[test]
                print ""
            print_separator()
            sys.exit(1)

def print_separator():
    print ""
    print "="*79, "\n"

if __name__ == "__main__":
    tests = None
    if len(sys.argv) > 1:
        tests = SimpleTests(sys.argv[1])
    else:
        tests = SimpleTests()

    print "\n================================"
    print "openLilyLib automated test suite"
    print "================================\n"
    print "Running LilyPond", tests.lilypond_version
    oll_dir = tests.openlilylib_dir
    print "OpenLilyLib directory: {}".format(oll_dir)

    print "LilyPond command to be used:"
    print " ".join(tests.lily_command_with_includes + ["<test-file>"])

    print "\nClear previous test results (if any)."
    tests.clean_results_dir()
    tests.run()
