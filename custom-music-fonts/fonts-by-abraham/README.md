LilyPond Music Fonts
====================

The fonts in this folder are provided as alternatives to the default 
*Emmentaler* music fonts to customize the look and feel of LilyPond's 
engraved scores. The OTF format is required for PDF output and the SVG 
format is required for SVG output. These files have NOT been tested on
versions older than 2.18.2, so any testing and feedback may be sent to
the author (see below).

In each font's folder has the following minimum items:

- License
- "otf" and "svg" folders, containing the aforementioned font files
- A font sampler sheet.

In the "otf" and "svg" folders, you will find multiple font files with the
name ``<font-name>-XX`` where ``XX`` is one of 11, 13, 14, 16, 18, 20, 23, 
and 26. Although *Emmentaler* (as currently known) is the only font that 
actually supports these optical font sizes, LilyPond expects to find each of
these files, so a separate file for each of the above sizes has been created
to satisfy this dependency.

Installing The Patch
--------------------

Before these font's can be used, LilyPond's ``font.scm`` file must be patched
with the file by the same name in this directory. The location of ``font.scm``
is OS dependent, but should be found in a directory like:

    <LilyPond-Install-Dir>/<version>/scm

**CAUTION**: Just in case the patched file doesn't work, you may want to save
the original ``font.scm`` file so that it can be put back.

The ``font.scm`` file in this repository patches the syntax of three functions 
in order to support alternate music fonts:

- ``add-music-fonts``
- ``make-pango-font-tree``
- ``make-century-schoolbook-tree``

The patch for ``add-music-fonts`` removes the requirement that the music font
name also have a piano brace font as well. This allows the user to use a 
separate piano brace font from the main notation font. As of 07/2014, the only
known brace fonts are *Emmentaler* and *Gonville*.

**NOTE**: The author of these font files has not attempted to create new piano
brace font files, only music notation font files. Hopefully a reliable method
for this can be created in the future.

Installing The Fonts
--------------------

To install the music font files, administrative privileges may be required. The
installation directory **is not the system font directory**. The correct
directory should be found in a directory like

    <LilyPond-Install-Dir>/<LilyPond-Version>/fonts

In this folder is found two folders: "otf" and "svg". Simply copy and paste
the corresponding music font files in this repository to those folders.

**NOTE**: Any regular text fonts SHOULD be installed in the normal system's 
font directory (i.e., the LilyJAZZ text and chord fonts).

Usage
-----

Most users will have only used ``make-pango-font-tree`` in their scores, so
here is how the syntax is changed. Instead of specifying only text fonts like

    #(define fonts 
      (make-pango-font-tree 
      <roman-font> 
      <sans-font> 
      <monospace-font> 
      factor))

in a ``\paper`` block, which sets up *Emmentaler* as the **music** and 
**brace** fonts under the covers, the new syntax puts these two options at 
the beginning of the argument list of ``make-pango-font-tree``:

    #(define fonts 
      (make-pango-font-tree 
      <music-font>
      <brace-font>
      <roman-font> 
      <sans-font> 
      <monospace-font> 
      factor))

**NOTE**: If the global staff size is to be changed, this must be done *BEFORE*
this ``\paper`` block using the syntax ``#(set-global-staff-size XX)`` where
XX is any number in units of pt (Default: 20).

For example, the following makes *Cadence* the global notation font, but uses
the *Gonville* piano brace font:

    \paper {
      #(define fonts
        (make-pango-font-tree
        "cadence"  ; the music font (all lower-case letters)
        "gonville"  ; the brace font (all lower-case letters)
        "Century Schoolbook L"  ; the main/roman (serif) font
        "FreeSans"  ; the sans-serif font
        "Inconsolata"  ; the monospace/typewriter font
        (/ staff-height pt 20)  ; the scaling factor
       ))
    }

Stylesheets and House-styles
----------------------------

To make using the music fonts more convenient, stylesheets have been created 
(with the "ily" extension) so you only have ``\include`` the stylesheet and 
the desired music font will automatically be used. In order to not have to make
unnecessary duplicates of stylesheets, it may be convenient to put the 
stylesheet in its own folder and include that directory in LilyPond's path 
variable. Once this is done, for example, in order to use the LilyJAZZ music
and text fonts, all that is necessary is to simply put

    \include "LilyJAZZ.ily"

near the top of the score. Other layout overrides may also be included in these
stylesheets. Other stylesheets (house-styles) can be created in a similar
manner.

To add the stylesheet directory, here's are some options:

 - **Frescobaldi**: Open up the preferences with "Edit" > "Preferences. Then, 
   under "LilyPond Preferences" you will find a section called "LilyPond 
   include path:". Click "Add" and type in the directory that contains the 
   stylesheets.

 - **Command-line**: Simply add the option ``--include=<directory>`` to add
   the *<directory>* that contains the stylesheet to LilyPond's search path.

 - **Relative-includes**: See the LilyPond documentation about other ways to
   [include files in other folders][1].

### Example

To test out the new fonts and stylesheets, two files (``test-sampler.ly`` and
``NALADA.ly``) have been provided with some instructions near the top about 
appropriate usage. Enjoy!

Future Work/New Music Fonts
---------------------------

If a new music font is desired, please send either images of the desired 
notation glyphs and/or suitable binary font file (TTF, OTF, SVG, etc.) or 
FontForge SFD file to the author. 

**NOTE**: If glyphs are provided in one of the above binary/FontForge forms, 
be aware that **250 em-unit = 1 staff-space** (i.e., the distance between 
staff lines). It may be helpful to use one of the Emmentaler font files as a 
reference for glyph size/positioning.

Questions and Feedback
----------------------

Any questions or feedback should be sent to the author of these files: 
[Abraham Lee](mailto:tisimst@gmail.com). Please consider the following before
contacting:

 - *Installation questions* should include the version of LilyPond being used
   as well as the operating system (OS). 
 - *Usage questions* should be in the form of short LilyPond snippets if it is
   possible, but full files are ok, if absolutely necessary.

You may also like to read through the author's 
[personal webpage](https://sites.google.com/site/tisimst), which gives a more
in-depth look at using custom fonts.

[1]: http://lilypond.org/doc/v2.18/Documentation/notation/including-lilypond-files

