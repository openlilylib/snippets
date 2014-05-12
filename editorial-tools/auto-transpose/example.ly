\version "2.18.2"
\include "editorial-tools/auto-transpose/definitions.ily"
\include "deutsch.ly"

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
    \remove "Key_engraver"
    \consists #autoTranspose
    \consists "Key_engraver"
    % if music and print are equal, do nothing
    % else transpose according to transp (up or down)
    music-concert-pitch = ##t
    print-concert-pitch = ##f
    % if music is given in instrument-pitch, but shall be printed in concert-pitch,
    %   midi pitch is false - instrumentTransposition should be "turned off" for midi(?)
  } {
    \key f \major
    \bach
    \instrumentSwitch "b-clarinet"
    \bach
    \instrumentSwitch "eb-clarinet"
    \bach
  }
  \layout {}
  \midi { \tempo 4=150 }
}


