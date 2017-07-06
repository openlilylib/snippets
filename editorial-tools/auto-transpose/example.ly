\version "2.19.15"

\include "deutsch.ly"

\include "oll-core/package.ily"
\loadModule snippets.editorial-tools.auto-transpose
%\include "editorial-tools/auto-transpose/definitions.ily"

% some music to insert into example
bach = \relative c'' { b a c h }

% add two transposing instrument-definitions
\addInstrumentDefinition #"eb-clarinet"
  #`((instrumentTransposition . ,(ly:make-pitch 0 2 -1/2))
     (shortInstrumentName . "Es-Kl")
     (clefGlyph . "clefs.G")
     (middleCPosition . -6)
     (clefPosition . -2)
     (instrumentCueName . "Es-Kl")
     (midiInstrument . "clarinet"))

\addInstrumentDefinition #"b-clarinet"
  #`((instrumentTransposition . ,(ly:make-pitch -1 6 -1/2))
     (shortInstrumentName . "Kl")
     (clefGlyph . "clefs.G")
     (middleCPosition . -6)
     (clefPosition . -2)
     (instrumentCueName . "Kl")
     (midiInstrument . "clarinet"))

%%% create demo score
\score {
  \new Staff \with {
    \autoTranspose
  } {
    \key f \major
    \bach
    \instrumentSwitch "b-clarinet"
    \key f \major
    \bach
    \instrumentSwitch "eb-clarinet"
    \key f \major
    \bach
  }
  \layout {}
  \midi { \tempo 4=150 }
}


