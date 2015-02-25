#!/usr/bin/env python

import os
import collections

# File configuring the requested LilyPond versions
lily_versions_file = "./test/LILYPOND-VERSIONS"

###########################
# Determine the environment
# This script doesn't have to be platform independent as it only runs on Linux machines

# Home directory, serves as root for several paths
home_dir = os.getenv("HOME")
# base directory where two LilyPond versions are installed and cached
install_root = "{}/.lilypond".format(home_dir)


def load_lily_versions():
    """Read requested LilyPond versions from a file.
       Return an ordered dictionary."""
    versions = collections.OrderedDict()
    with open(lily_versions_file, 'r') as versions_file:
        for line in versions_file.readlines():
            version_line = line.strip()
            # The following "parsing" could use more care ;-)
            if not version_line.startswith("#") and len(version_line) > 0:
                target, version = version_line.split('=')
                versions[target] = version
    return versions

def print_separator():
    print ""
    print "="*79, "\n"

