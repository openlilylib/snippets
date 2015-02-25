#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys
import collections

import common_functions
from common_functions import print_separator, home_dir, install_root

#############################################################
# Load environment variables
# at the same time checking if we're running on the CI server

try:
    is_ci = os.environ["CI"]
    lily_platform = os.environ["LILY_PLATFORM"]
    lily_version = os.environ["LILY_VERSION"]
except:
    sys.exit('\nScript can only be run in CI mode. Aborting\n')

#########################
# Configuration constants

# Download site for LilyPond distributions
binary_site = "http://download.linuxaudio.org/lilypond/binaries/"
# String template for generating the LilyPond installation command
lily_install_script = "lilypond-install.sh"


#################################
# Functions doing the actual work

def download_url():
    """Format a string representing the URL to download the requested LilyPond distribution"""
    return "{}{}/lilypond-{}.{}.sh".format(
        binary_site, lily_platform, lily_version, lily_platform)

def install_distribution():
    """Download and install LilyPond version if not cached"""
    lilypond_cmd = os.path.join(install_root,
                                "bin/lilypond")
    print "\nChecking LilyPond presence with {}\n".format(lilypond_cmd)
    try:
        sp.check_call([lilypond_cmd, '--version'])
        print "LilyPond {} is already installed in cache, continuing with test script.".format(lily_version)
    except:
        print "LilyPond {} is not installed yet.".format(lily_version)
        print "Downloading and installing now"
        sp.check_call(
            ["wget", "-O",
             lily_install_script,
             download_url()])
        sp.check_call(["sh", lily_install_script,
                       "--prefix",
                       install_root,
                       "--batch"])

#########################
# Actual script execution

if __name__ == "__main__":
    print_separator()
    print "============================="
    print "openLilyLib automated testing"
    print "============================="
    print "Step 1:"
    print "check LilyPond installation."
    print "Requested LilyPond version: {}".format(lily_version)

    install_distribution()
