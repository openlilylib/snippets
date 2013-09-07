\version "2.16.2"

\markup \justify {
  Why there is no sound in midi output? It appears that
  \typewriter { midiInstrument = "acoustic grand" }
  from instrument-context-definitions isn't applied.
}

\include "instrument-context-definitions.ily"

\score {
  <<
    \new SopranoVoice = sop { c' b' a' f' }
    \new Lyrics \lyricsto sop \lyricmode { la la la la }
  >>
  \layout {  }
  \midi { }
}
