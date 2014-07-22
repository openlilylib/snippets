\version "2.16.2"

\header {
  status="unfinished"
}

\include "instrument-context-definitions.ily"

sopranomelody = \relative c'' {
  c b a f
}
altomelody = \relative f' {
  f g a c,
}
\score {
  \new ChoirStaff <<
    % FIXME
    % two staves appear, while only one is wanted.
    % probably i have to add WomenStaff definition
    \new SopranoStaff <<
      \new SopranoVoice {
        \voiceOne
        \sopranomelody
      }
      \new AltoVoice {
        \voiceTwo
        \altomelody
      }
    >>
  >>
  \layout {
    \override Staff.NoteHead #'color = #blue
    \override AltoStaff.NoteHead #'color = #red
    \override SopranoVoice.NoteHead #'color = #green
  }
  \midi {
    \set SopranoVoice.midiInstrument = #"distorted guitar"
    \set AltoVoice.midiInstrument = #"clarinet"
  }
}
