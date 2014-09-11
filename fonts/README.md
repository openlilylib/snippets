Installation script for additional LilyPond fonts
=================================================

This is a script intended to make it easier to "install" the additional
fonts provided by Abraham Lee into a given LilyPond installation (see the
[Home Page](http:///fonts.openlilylib.org).

The idea is *not* to copy the font files into the font directories of
a LilyPond installation but to store them at any convenient location
and only place symbolic links. This improves the usage of the fonts
in multiple parallel installations.

**Usage:**  
`python install-lily-fonts.py INDIR LILYDIR`

- `INDIR` is a directory containing the fonts  
  The path can be given absolute or relative to the working directory.  
  The directory will be processed recursively so you can install *all*
  fonts present in subdirectories at once.
- `LILYDIR` is the root directory of a LilyPond installation.
  Depending on your operating system it should be the directory called
  `lilypond` or `LilyPond`, containing the `usr` subdirectory.  
  Additionally it works with custom builds, passing the root of the
  build-directory.

The script checks both arguments for plausibility and in case of doubt
it prints a message and aborts.

The INDIR is traversed recursively, and any found .otf or .svg file will
be symlinked to the respective places in LILYDIR.

If a symlink can't be created (for example because there already
exists one) a message is printed and the font is skipped.

**Restrictions:**

- Currently the script doesn't support LilyPond installations through a
  Linux package manager.
- Probably it does *not* work on Windows as there is no symlink support
  there (suggestions and patches welcome)
- The directory structure on Mac OS X hasn't been implemented yet.

