#!/usr/bin/env python
#-*- coding:utf-8 -*-

# This file is part of the Gruppen project
# https://git.ursliska.de/openlilylib/gruppen
#
# Copyright (c) 2014 by Urs Liska and others
# (as documented by the Git history)
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,   
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# See http://www.gnu.org/licenses/ for more information.


"""
Command line parsing.
"""

import sys
import os
import argparse


scriptname = "install-lily-fonts"

# instantiate command line parser
parser = None
def create_parser():
    global parser
    parser = argparse.ArgumentParser(
        usage = ("{} [options]").format(scriptname),
        version = "{appname} {scriptn} {version}".format(
            appname = "LilyPond-Extra-Fonts", 
            scriptn = scriptname, 
            version = "0.1"),
        description = ("Install extra fonts for LilyPond from http://fonts.openlilylib.org"))
        
    parser.add_argument('-d', '--font-directory', 
        default = os.getcwd(), 
        help=("Root directory where all fonts are stored.\n" +
              "Defaults to the current working directory."))
    parser.add_argument('-e', '--lilypond-executable', 
        help=("Path (absolute or relative to current working directory) " +
              "LilyPond executable. If not given some attempts are done " +
              "to determine it automatically."))
    # not implemented yet
    parser.add_argument('-b', '--batch', 
        help=("Run in batch mode. This means:\n" +
              "- In case of doubts don't ask but abort the script\n" +
              "- Don't ask for choices but install/update all found fonts\n" +
              "- Quietly overwrite existing files"))
    # not implemented yet
    parser.add_argument('-f', '--force', 
        help=("Don't try to determine updated fonts, overwrite everything " +
              "with upstream files."))
    # not implemented yet
    parser.add_argument('-l', '--local', 
        help=("Consider the local font catalog up to date, don't download " +
              "the remote one. Useful for updating multiple LilyPond installations."))
                
    # Make sure debugger options are recognized as valid. These are passed automatically
    # from PyDev in Eclipse to the inferior process.
    if "pydevd" in sys.modules:
        parser.add_option('-v', '--vm_type')
        parser.add_option('-a', '--client')
        parser.add_option('-p', '--port')
        parser.add_option('-f', '--file')
        parser.add_option('-o', '--output')

create_parser()


def parse():
    """Parse command line and return options and files (if applicable).
    Check project directory along the way (set project properties
    or raise an exception)."""

    args = sys.argv
    if os.name == 'nt' and args and 'python' in os.path.basename(args[0]).lower():
        args = args[2:]
    else:
        args = args[1:]
    args = vars(parser.parse_args(args))
    
    return args
