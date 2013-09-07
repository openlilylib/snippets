\version "2.16.2"

\markup \justify {
  Below i've defined custom contexts: SopranoStaff and SopranoVoice.
  However, they don't work with midi - first i get a
  \typewriter "warning: cannot find or create new `SopranoVoice'"
  and ten "\lyricsto" fails, saying that it \typewriter "cannot find Voice `sop'".
  I suppose that because of some Midi bug a Voice is created instead of
  SopranoVoice, and then "\lyricsto" cannot find the SopranoVoice
  it is supposed to find.

  Without "\midi" block eveerything works fine.
}

\layout {
  \context {
    \ChoirStaff
    \accepts "SopranoStaff"
  }

  \context {
    \Staff
    \type "Engraver_group"
    \name "SopranoStaff"
    \alias "Staff"

    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"

    instrumentName = "Soprano"
  }

  \context {
    \Voice
    \name "SopranoVoice"
    \alias "Voice"
  }
}

\score {
  <<
    \new SopranoVoice = sop { c' b' a' f' }
    \new Lyrics \lyricsto sop \lyricmode { la la la la }
  >>
  \layout {  }
  \midi { }
}
