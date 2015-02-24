#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys
import collections

#############################################################
# Load environment variables
# at the same time checking if we're running on the CI server

try:
    is_ci = os.environ["CI"]
    lily_platform = os.environ["LILY_PLATFORM"]
    lily_version_key = os.environ["LILY_VERSION"]
except:
    sys.exit('\nScript can only be run in CI mode. Aborting\n')

#########################
# Configuration constants

# Download site for LilyPond distributions
binary_site = "http://download.linuxaudio.org/lilypond/binaries/"
# File configuring the requested LilyPond versions
lily_versions_file = "./test/LILYPOND-VERSIONS"
# String template for generating the LilyPond installation command
lily_install_script = "lilypond-install-{}.sh"

###########################
# Determine the environment
# This script doesn't have to be platform independent as it only runs on Linux machines

# Home directory, serves as root for several paths
home_dir = os.getenv("HOME")
# base directory where two LilyPond versions are installed and cached
install_root = "{}/.lilypond".format(home_dir)


#################################
# Functions doing the actual work

def download_url(version):
    """Format a string representing the URL to downolad the requested LilyPond distribution"""
    return "{}/{}/lilypond-{}.{}.sh".format(
        binary_site, lily_platform, version, lily_platform)

def install_distributions(versions):
    for v in versions:
        vstring = versions[v]
        lilypond_cmd = os.path.join(install_root,
                                    vstring,
                                    "bin/lilypond")
        try:
            print "\nChecking LilyPond presence with {}\n".format(lilypond_cmd)
            sp.check_call([lilypond_cmd, '--version'])
            print "LilyPond {} already installed".format(vstring)
        except:
            print "Downloading and installing LilyPond {}".format(vstring)
            install_script = lily_install_script.format(vstring)
            sp.check_call(
                ["wget", "-O",
                 install_script,
                 download_url(vstring)])
            sp.check_call(["sh", install_script,
                "--prefix",
                os.path.join(install_root, vstring),
                "--batch"])

def load_lily_versions():
    """Read requested LilyPond versions from a file.
       Return an ordered dictionary."""
    versions = collections.OrderedDict()
    with open(lily_versions_file, 'r') as versions_file:
        for line in versions_file.readlines():
            version_line = line.strip()
            if not version_line.startswith("#") and len(version_line) > 0:
                target, version = version_line.split('=')
                versions[target] = version
    return versions


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
    versions = load_lily_versions()
    print "Requested LilyPond versions:"
    for v in versions:
        print "{} = {}".format(v, versions[v])

    # Clean up
    remove_previous_lilyponds(versions)

    # Install LilyPonds if necessary
    install_distributions(versions)
