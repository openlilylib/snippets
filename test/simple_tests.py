#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys
import re

class SimpleTests:
    """Run simple integration tests. Specifically, this script will look
    for all the `.simple-tests` files in the openLilyLib directory
    tree. For each such file, each line not starting with `#` is
    treated as a filename. All these files will be compiled with
    LilyPond. If the compilation results in a non-zero exit code, then
    that test is marked as failed.

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


    def __init__(self, cmd=None):
        self.clean_tmp_dir()
        if self.is_ci_run():
            self.__install_distribution()
            self.lily_command = osp.join(self.tmp_lily_dir, "bin/lilypond")
        elif not cmd == None:
            self.lily_command = cmd
        else:
            raise Exception("The lilypond command should be given on the command " +
                            "line or configured via environment variables")

        self._openlilylib_dir = ""

        self.lilypond_version = self.__lilypond_version()

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
        return re.search(r"\d\.\d\d\.\d", version_line).group(0)

    def is_runnable_file(self, fname):
        """Returns true if fname can be compiled with the lilypond version used"""
        with open(fname, 'r') as fcontents:
            for line in fcontents.readlines():
                version_line = re.search(r"\\version \"(\d\.\d\d\.\d)\"", line)
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

    def openlilylib_dir(self):
        if not self._openlilylib_dir:
            script_path = osp.abspath(osp.dirname(osp.realpath(__file__)))
            self._openlilylib_dir = osp.abspath(osp.join(script_path, os.pardir))
        return self._openlilylib_dir

    def __collect_tests(self):
        test_files = []
        for root, _, files in os.walk(self.openlilylib_dir()):
            for f in files:
                fname = osp.join(root, f)
                if f == self.test_list_fname:
                    print "Found file listing simple tests:", fname
                    with open(fname, 'r') as test_list:
                        for line in test_list.readlines():
                            test_fname = line.strip()
                            if not line.startswith("#") and len(test_fname) > 0:
                                test_file = osp.join(root, test_fname)
                                if self.is_runnable_file(test_file):
                                    test_files.append(test_file)
        return test_files

    def run(self):
        failed_tests = []
        all_tests = self.__collect_tests()
        for test in all_tests:
            print "\n\nRunning test", test
            cmd = [self.lily_command,
                   "-I", self.openlilylib_dir(),
                   "-I", os.path.join(self.openlilylib_dir(), "ly"),
                   test]
            print "Command: ", " ".join(cmd)
            lily = sp.Popen(cmd,
                            stdout=sp.PIPE, stderr=sp.PIPE)
            (out, err) = lily.communicate()
            if lily.returncode != 0:
                failed_tests.append(test)
                print "\n====== FAILED ======"
                print err
                print "---------------------"
            else:
                print "------- OK! --------"
        print "="*79, "\n"
        print "  {} failed tests out of {}".format(
            len(failed_tests), len(all_tests)), "\n"
        print "="*79, "\n"
        if len(failed_tests) > 0:
            print "Failed tests"
            for test in failed_tests:
                print " ", test
            print ""
            print "="*79
            sys.exit(1)


if __name__ == "__main__":
    tests = None
    if len(sys.argv) > 1:
        tests = SimpleTests(sys.argv[1])
    else:
        tests = SimpleTests()
    print "Running LilyPond", tests.lilypond_version
    oll_dir = tests.openlilylib_dir()
    print "OpenLilyLib directory", oll_dir
    tests.run()
