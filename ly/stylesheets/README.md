Stylesheets
===========

A library to manage font selection and stylesheets with the [GNU LilyPond](http://lilypond.org) music typesetter.
This library is part of [openLilyLib](https://github.com/openlilylib/openlilylib) and maintained by

- Urs Liska (ul@openlilylib.org)
- Kieren MacMillan (kieren_macmillan@sympatico.ca)
- Abraham Lee (tisimst.lilypond@gmail.com)

---

Initially only loading of alternative fonts for LilyPond is implemented, but this is already a great enhancement.
On http://fonts.openlilylib.org a comprehensive range of alternative notation fonts is available for download. 
If you have installed `openLilyLib` you can ignore the documentation with regard to the *usage* of the fonts,
once you have downloaded and installed them. Very soon a Python script will available as part of this library
that will make the process of downloading, updating and "installing" the fonts a nearly automatic process too.

Using Alternative Fonts
-----------------------

The easiest way to use an alternative notation font in LilyPond is:

```lilypond
\version "2.19.12"

\include "openlilylib"
\loadModule "stylesheets"
\useNotationFont Cadence
```

which will set up everything correctly to use "Cadence" as your document's notation font.
Font names are case insensitive (so other than with the manual activation you don't need to
write `cadence` here, and note that when the font name doesn't contain "illegal" characters
(which currently only is the case with `Gutenberg1939`) you don't need quotation marks.

The "simple" form of `\useNotationFont` uses the same name for notation and brace fonts and loads
a default stylesheet that accompanies each font, adjusting LilyPond's engraving settings (e.g.
line thicknesses) to match the appearance of the font. However, you have more fine-grained control
by using the following form:

```lilypond
\useNotationFont \with {
  brace = Beethoven
  style = none
  extensions = ##t
}
Cadence
```

This will set the brace font to `Beethoven` and skip loading of a stylesheet. Using `none` for the
`brace` option will use the default Emmentaler brace font, which is usually a good idea when the
notation font does not have a corresponding brace font (which is currently the case with the 
*Cadence*, *Paganini* and *Scorlatti* fonts).

Using `none` as `style` will skip loading a stylesheet, which you may want when creating a style sheet
from scratch. Please consult the documentation about any additional styles available for a given font.
if a non-existent stylesheet is requested a warning is issued and the default stylesheet is loaded.

A font may contain extensions that can be activated with the `extensions = ##t` option. Currently only
the *Arnold* fonts has such extensions, consisting of a few extra articulations and commands.
Requesting extensions for a font that doesn't provide them will issue a warning but don't do any further harm.
