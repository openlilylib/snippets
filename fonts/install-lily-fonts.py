#!/usr/bin/env python

#-*- coding:utf-8 -*-

from __future__ import unicode_literals

import  os, sys

# define global variables
font_dir = ''
lily_dir = ''
otf_dir = ''
svg_dir = ''


def main():
    if not check_dirs():
        usage()
        sys.exit(1)
    
    walk_indir()
    
def check_dirs():
    """Check if two arguments are given and point to
    valid directories"""
    global font_dir, lily_dir, otf_dir, svg_dir

    if len(sys.argv) < 3:
        return False
        
    # check the given font directory (only presence)
    font_dir = normalize_dir(sys.argv[1])
    if not font_dir:
        return False

    # check the presence of the given LilyPond directory
    lily_dir = normalize_dir(sys.argv[2])
    if not lily_dir:
        return False
    
    # check if we have a LilyPond install or custom build dir
    base = 'out' if 'out' in os.listdir(lily_dir) else 'usr'
        
    font_base_dir = os.path.join(lily_dir, 
                           base, 
                           'share', 
                           'lilypond', 
                           'current', 
                           'fonts')

    otf_dir = os.path.join(font_base_dir, 'otf')
    svg_dir = os.path.join(font_base_dir, 'svg')
    if not os.path.isdir(otf_dir):
        print "Not a LilyPond installation?", lily_dir
        return False

    return True
    
def normalize_dir(path):
    """Returns the normalized absolute path while
    respecting if it is absolute or relative."""
    result = os.path.normpath(path) if os.path.isabs(path) else os.path.normpath(os.path.join(os.getcwd(), path))
    if not os.path.isdir(result):
        print 'Not a directory:', result
        return ''
    return result
    
def walk_indir():
    """Recursively walk through the input dir
    and 'install' symlinks to any found .otf or .svg font files."""
    for dirName, subdirList, fileList in os.walk(font_dir):
        print "Processing dir:", dirName
        for f in fileList:
            link_name = ''
            name, ext = os.path.splitext(f)
            if ext == '.otf':
                link_name = os.path.join(otf_dir, f)
            if ext == '.svg':
                link_name = os.path.join(svg_dir, f)
            if link_name:
                link_target = os.path.join(dirName, f)
                try:
                    os.symlink(link_target, link_name)
                except OSError, e:
                    print "Could not install font:", link_target
                    print str(e)
                print 'Installed:', link_name
            
    
def usage():
    print ('Usage:\n' +
           'install-lily-fonts.py FONTDIR LILYDIR\n\n' +
           'Both FONTDIR and LILYDIR must be existing directories\n' +
           'and LILYDIR must be the root of a LilyPond installation.')
    

# ####################################
# Finally launch the program
if __name__ == "__main__":
    main()
