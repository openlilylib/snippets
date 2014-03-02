\version "2.18.0"

\include "definitions.ily"

music = \relative c' {
  \clef alto
  \time 3/4
  c4-.(\f\< d4-. es4-.) |
  \time 4/4
  fis8.---\trill\sfz\> e!16\downbow d16->\niente r16 r8 c2-\prall |
  \time 2/2
  \clef treble
  r2-\fermata c8( eeh8)-^ \tuplet 3/2 { eeh8( gisih8 b')-! } |
  c,,4. c8 c4.. c16 |
}

<<
  \new Staff \with { instrumentName = "Feta" } \music
  \new Staff \with { \bravuraOn instrumentName = "Bravura" } \music
>>

\markup "Create a ligature of two glyphs with \\smufllig"

\markup { \smufllig #'("gClefLigatedNumberAbove" "tuplet4") }
