#!/usr/bin/env python

import subprocess as sp
import os
import os.path as osp
import shutil
import sys

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
    versions = {}
    with open("./test/LILYPOND-VERSIONS", 'r') as versions_file:
        for line in versions_file.readlines():
            version_line = line.strip()
            if not version_line.startswith("#") and len(version_line) > 0:
                target, version = version_line.split('=')
                versions[target] = version
    return versions


def remove_previous_lilyponds(targets):
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
    print "LilyPond installation script for"
    print "automtated testing of openLilyLib on travis-ci.org\n"
    print "\nLoading LilyPond versions configuration"
    versions = load_lily_versions()
    print "Found versions:"
    print "\n".join([versions[v] for v in versions])
    remove_previous_lilyponds(versions)
    install_distributions(versions)

