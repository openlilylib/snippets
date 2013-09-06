\version "2.16.2"

\include "instrument-context-definitions.ily"

sopranomelody = \relative c'' {
  c b a f
}
altomelody = \relative f' {
  f g a c,
}
tenormelody = \relative c' {
  c a c c
}
bassmelody = \relative f {
  f e d c
}
text = \lyricmode {
  la la la la
}

\score {
  \new ChoirStaff <<
    \new SopranoStaff \sopranomelody
    \addlyrics \text

    \new AltoStaff \altomelody
    \addlyrics \text

    \new TenorStaff \tenormelody
    \addlyrics \text

    \new BassStaff \bassmelody
    \addlyrics \text
  >>
  \layout {
    \override Staff.NoteHead #'color = #blue % this works
    \override AltoStaff.NoteHead #'color = #red % this works
    \override SopranoVoice.NoteHead #'color = #green % doesn't work!
    % apparently no SopranoVoice is created, but just a regular Voice.
    % why? i've used \defaultchild "SopranoVoice" in SopranoStaff definition.
  }
}
