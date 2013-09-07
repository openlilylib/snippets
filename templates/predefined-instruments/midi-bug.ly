\version "2.16.2"

% everything works now!

\include "instrument-context-definitions.ily"

\score {
  <<
    \new SopranoVoice = sop { c' b' a' f' }
    \new Lyrics \lyricsto sop \lyricmode { la la la la }
  >>
  \layout {  }
  \midi { }
}
