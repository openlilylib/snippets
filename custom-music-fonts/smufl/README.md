[SMuFL](http://smufl.org/) is a new proposed standard for music fonts, formed alongside Steinberg's in-development notation software. The first and currently the only SMuFL font is Bravura, a bold and easy-to-read typeface developed for Steinberg's program by Daniel Spreadbury. It is freely available under the SIL Open Font License.

This is a LilyPond script providing partial support for Bravura and other future SMuFL fonts. It is a heavily modified derivative of LilyJAZZ.

## Usage ##

**Current version:** SMuFL 1.12

#### Installing font files ####

You will need a copy of the Bravura font first. You can either take the files from the `bravura-1.0` folder or from
[the SMuFL website](http://www.smufl.org/fonts/).  Install the font by copying `bravura-1.0/otf/Bravura.otf` into the directory `/usr/share/lilypond/<version>/fonts/otf/` of your LilyPond installation.  (If you are running lilypond
from source code, copy the file to `out/share/lilypond/current/fonts/otf/` subdir of lilypond build).


#### Using the font in LilyPond files ####

Ensure that the openLilyLib snippets repository is present locally and in Lilypond's search path.
Include `custom-music-fonts/smufl/definitions.ily` and add `\bravuraOn` (or `\smuflOn` to avoid Bravura-specific overrides) to the Staff context:

```lilypond
\include "custom-music-fonts/smufl/definitions.ily"

\new Staff {
  \bravuraOn
  c'4 d' e' c'
}
```

Or, better:

```lilypond
\include "custom-music-fonts/smufl/definitions.ily"

\score {
  % ...
  \context {
    \Staff {
      \bravuraOn
    }
  }
}
```

With this, many music features will be automatically converted into the Bravura font. Compilation will be a little slower, espcially for large scores.

To revert things back to normal, use `\bravuraOff` or `\smuflOff`.

## Commands ##

To invoke a glyph by name like `\musicglyph`, use the markup command `\smuflglyph`. `\smuflglyph #"segno"`, for example, will print a segno sign in the Bravura font. `\smuflglyph` is **not** compatible wih `\musicglyph`, and manual conversion will be necessary.

`\smuflglyph-compat` works exactly like `\smuflglyph`, but if the string passed to it is not a valid SMuFL name, it will behave as `\musicglyph`.

`\smuflchar` is the same as `\smuflglyph`, but it takes a Unicode code point in place of a glyph name.

`\smufllig` takes a list of glyph names (in the form of strings, not symbols) and concatenates them together. This is useful because it can create ligatures; there is no way to do so using the other commands:

    % Wrong:
    \concat { \smuflglyph #"gClefLigatedNumberAbove" \smuflglyph #"tuplet5" }
    % Right:
    \smufllig #("gClefLigatedNumberAbove" "tuplet5")

Some new dynamics are added, such as `\pppppp`, `\ffffff`, and `\niente`.

If you want access to LilySMuFL's new commands, but don't want to slow down compilation for now, leave the include in place and comment out `\bravuraOn` or `\smuflOn`. Now you can use `\smuflglyph`, etc. while keeping compilation speedy. There is a caveat: if you use `\niente`, add this line where you would put `\bravuraOn`:

    \override Staff.DynamicText.stencil = #smufl-dynamic-text

Without it, `\niente` will look a bit strange.

## Conversion (for developers) ##

Provided is a script `glyphnames.py` for converting glyph JSON data into a LilyPond-readable Scheme format. This was necessary for every 0.x update, but it will probably not be needed until a large update like 2.0. To use the script in an update, download the JSON metadata file from [the SMuFL website](http://www.smufl.org/download/), replace the existing `glyphnames.json` in this directory, and run `python3 glyphnames.py`. This will update `smufldata.ily`. You may need to delete `~/.lilypond-fonts.cache-2/` if LilyPond has trouble recognizing the new font.

## To-do list ##

LilySMuFL has a few defects. A few make it not yet usable for professional engraving, and others are simply Feta features not converted to SMuFL yet.

 * Styles not supported for rests, noteheads, or flags
 * Strange Script alignment at times (fermatas, staccatos)
 * Occasional ugly Accidental spacing
 * Still using Feta: Arpeggio, BreathingSign, OttavaBracket, PercentRepeat, TrillSpanner, TupletNumber, StemTremolo, SustainPedal
