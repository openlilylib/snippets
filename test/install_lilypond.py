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
lily_install_script_tmpl = "lilypond-install-{}.sh"


#################################
# Functions doing the actual work

def download_url():
    """Format a string representing the URL to download the requested LilyPond distribution"""
    return "{}{}/lilypond-{}.{}.sh".format(
        binary_site, lily_platform, lily_version, lily_platform)

def install_distribution():
    """Download and install LilyPond version if not cached"""
    lilypond_cmd = os.path.join(install_root,
                                lily_version,
                                "bin/lilypond")
    try:
        print "\nChecking LilyPond presence with {}\n".format(lilypond_cmd)
        sp.check_call([lilypond_cmd, '--version'])
        print "LilyPond {} is already installed in cache, continuing with test script.".format(lily_version)
    except:
        print "Downloading and installing LilyPond {}".format(lily_version)
        install_script = lily_install_script_tmpl.format(lily_version)
        sp.check_call(
            ["wget", "-O",
             install_script,
             download_url()])
        sp.check_call(["sh", install_script,
                       "--prefix",
                       os.path.join(install_root, lily_version),
                       "--batch"])


def remove_previous_lilyponds(targets):
    """Check the requested LilyPond versions against the
       cached ones on the server. If an installed version is
       found that isn't used for testing it will be removed.
       This is our 'garbage collection' for new LilyPond versions."""
    if not os.path.exists(install_root):
        os.mkdir(install_root)
        return
    installed_dirs = os.listdir(install_root)
    target_dirs = [targets[d] for d in targets]
    for d in installed_dirs:
        if not d in target_dirs:
            print "Remove obsolete LilyPond installation {}\n".format(d)
            shutil.rmtree(os.path.join(install_root, d))



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
