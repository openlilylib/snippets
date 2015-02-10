#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys

class SimpleTests:
    """\
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

    test_files = []

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

    def lilypond_version(self):
        lily = sp.Popen([self.lily_command, "-v"], stdout=sp.PIPE, stderr=sp.PIPE)
        return lily.communicate()[0].splitlines()[0]

    def openlilylib_dir(self):
        script_path = osp.abspath(osp.dirname(osp.realpath(__file__)))
        return osp.abspath(osp.relpath("..", script_path))

    def collect_tests(self):
        pass

if __name__ == "__main__":
    tests = None
    if len(sys.argv) > 1:
        tests = SimpleTests(sys.argv[1])
    else:
        tests = SimpleTests()
    print "Running", tests.lilypond_version()
    print "OpenLilyLib directory", tests.openlilylib_dir()
