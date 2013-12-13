[SMuFL](http://smufl.org/) is a new proposed standard for music fonts, formed alongside Steinberg's in-development notation software. The first and currently the only SMuFL font is Bravura, a bold and easy-to-read typeface developed for Steinberg's program by Daniel Spreadbury. It is freely available under the SIL Open Font License.

[LilyPond](http://lilypond.org/) is a free and open-source music engraving program. This is a LilyPond script providing partial support for Bravura and other future SMuFL fonts. It is a heavy modified derivative of LilyJAZZ.

Perhaps someday the Feta font will be made SMuFL-compatible so Feta can be used in other software.

## Usage ##

**Current version:** SMuFL 0.7

Make sure Bravura is installed on your system (get it from [the SMuFL website](http://www.smufl.org/fonts/)), and  that the openLilyLib snippets repository is present locally and in Lilypond's search path.
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

## Commands ##

To invoke a glyph by name like `\musicglyph`, use the markup command `\smuflglyph`. `\smuflglyph #"segno"`, for example, will print a segno sign in the Bravura font. `\smuflglyph` is **not** compatible wih `\musicglyph`, and manual conversion may be necessary. Sorry!

A few other new commands are added, such as the dynamics `\pppppp`, `\ffffff`, and `\niente`.

If you want access to LilySMuFL's new commands, but don't want to slow down compilation for now, leave the include in place and comment out `\bravuraOn` or `\smuflOn`. Now you can use `\smuflglyph`, etc. while keeping compilation speedy. There is a caveat: if you use `\niente`, add this line where you would put `\bravuraOn`:

    \override Staff.DynamicText.stencil = #smufl-dynamic-text

Without it, `\niente` will look a bit strange.

## Updating ##

SMuFL regularly changes its character codes with each update, and LilySMuFL is ideally forward-compatible with these changes. To try out a SMuFL update, first update the font installed on your system. Then download the JSON metadata file from [the SMuFL website](http://www.smufl.org/download/), replace the existing `glyphnames.json`, and run `python3.3 glyphnames.py`. This will update `smufldata.ily`, and will hopefully not break.

You may need to delete `~/.lilypond-fonts.cache-2/` if LilyPond has trouble recognizing the new font.

## To-do list ##

LilySMuFL has a few defects. A few make it not yet usable for professional engraving, and others are simply Feta features not converted to SMuFL yet.

 * Long dynamics (ppppp) are cut off to the right
 * Styles not supported for rests, noteheads, or flags
 * Strange Script alignment at times (fermatas, staccatos)
 * Occasional ugly Accidental spacing
 * Still using Feta: Arpeggio, BreathingSign, Dots, OttavaBracket, PercentRepeat, TrillSpanner, TupletNumber, StemTremolo, SustainPedal
