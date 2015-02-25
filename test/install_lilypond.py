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

def download_url(version):
    """Format a string representing the URL to download the requested LilyPond distribution"""
    return "{}/{}/lilypond-{}.{}.sh".format(
        binary_site, lily_platform, version, lily_platform)

def install_distributions(versions):
    """Download and install LilyPond versions if they are not cached"""
    for v in versions:
        vstring = versions[v]
        lilypond_cmd = os.path.join(install_root,
                                    vstring,
                                    "bin/lilypond")
        try:
            print "\nChecking LilyPond presence with {}\n".format(lilypond_cmd)
            sp.check_call([lilypond_cmd, '--version'])
            print "LilyPond {} is already installed".format(vstring)
        except:
            print "Downloading and installing LilyPond {}".format(vstring)
            install_script = lily_install_script_tmpl.format(vstring)
            sp.check_call(
                ["wget", "-O",
                 install_script,
                 download_url(vstring)])
            sp.check_call(["sh", install_script,
                "--prefix",
                os.path.join(install_root, vstring),
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
    print "Check cached LilyPond installations."

    print "\nLoading configuration ..."
    versions = common_functions.load_lily_versions()
    print "Requested LilyPond versions:"
    for v in versions:
        print "{} = {}".format(v, versions[v])

    # Clean up
    remove_previous_lilyponds(versions)

    # Install LilyPonds if necessary
    install_distributions(versions)
