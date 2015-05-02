#!/usr/bin/env python

import os
import sys

from lilycmd import LilyCmd
from common_functions import print_separator

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
# Actual script execution

if __name__ == "__main__":
    print_separator()
    print "============================="
    print "openLilyLib automated testing"
    print "============================="
    print "Step 1:"
    print "check LilyPond installation."
    print "Requested LilyPond version: {}".format(lily_version)

    LilyCmd.install(lily_platform, lily_version)
