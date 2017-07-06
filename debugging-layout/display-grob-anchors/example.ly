\version "2.16.2"

\include "oll-core/package.ily"
\loadModule snippets.debugging-layout.display-grob-anchors
%\include "./definitions.ily"

mus =
{
  \override NoteHead #'style = #'altdefault
  g'2->
  % Testing if \printRefpoint works with a custom-override.
  \once \override Script #'stencil =
  #(lambda (grob)
     (ly:font-get-glyph (ly:grob-default-font grob) "scripts.coda"))

  c''\fermata |
  as'1^"Yogi" |
  b'\breve _"Larry" |
  \mark "Twinkle" e''8 s4.
  \bar "|."
}


\markup "Red dots added for TimeSignature, Script, BarLine"
\new Staff \with { \printAnchors #'(TimeSignature Script BarLine) } \mus

\markup "Red dots added for all grobs"
\new Staff \with { \printAnchors #'all-grobs } \mus

\markup "Red dot added once to Script using \\onceDotScript"
{
  \override NoteHead #'style = #'altdefault
  g'2->
  \onceDotScript
  % Testing if \printRefpoint works with a custom-override.
  \once \override Script #'stencil =
  #(lambda (grob)
     (ly:font-get-glyph (ly:grob-default-font grob) "scripts.coda"))
  c''\fermata |
  as'1^"Yogi" |
  b'\breve _"Larry" |
  \mark "Twinkle" e''8-- s4.
  \bar "|."
}
