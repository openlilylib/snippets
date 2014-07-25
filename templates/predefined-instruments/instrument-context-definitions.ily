\version "2.19.10"

\include "context-creating-function.ily"

\layout {
  \newInstrument "Vocal" \Staff \Voice "" \ChoirStaff
  \with {
    \consists "Ambitus_engraver"
    instrumentName = "Vocals"
    shortInstrumentName = "Voc."
    \dynamicUp
    \tupletUp
  }
  \with { }
}
% Why i cannot put definitions of multiple contexts in one \layout????
\layout {
  \newInstrument "Soprano" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
  }
  \with { }
}
\layout {
  \newInstrument "Alto" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Alto"
    shortInstrumentName = "A"
    \clef G
  }
  \with { }
}
\layout {
  \newInstrument "Tenor" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Tenor"
    shortInstrumentName = "T"
    \clef "G_8"
  }
  \with { }
}
\layout {
  \newInstrument "Bass" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
  }
  \with { }
}

% ugh, this should be handled by newInstrument function, too.
\midi {
  \context {
    \ChoirStaff
    \accepts "VocalStaff"
    \accepts "SopranoStaff"
    \accepts "AltoStaff"
    \accepts "TenorStaff"
    \accepts "BassStaff"
  }

  \context {
    \Staff
    \name "VocalStaff"
    \alias "Staff"
    \accepts "VocalVoice"
    \defaultchild "VocalVoice"
    \remove "Staff_performer"
  }
  \context {
    \VocalStaff
    \name "SopranoStaff"
    \alias "VocalStaff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"
  }
  \context {
    \VocalStaff
    \name "AltoStaff"
    \alias "VocalStaff"
    \accepts "AltoVoice"
    \defaultchild "AltoVoice"
  }
  \context {
    \VocalStaff
    \name "TenorStaff"
    \alias "VocalStaff"
    \accepts "TenorVoice"
    \defaultchild "TenorVoice"
  }
  \context {
    \VocalStaff
    \name "BassStaff"
    \alias "VocalStaff"
    \accepts "BassVoice"
    \defaultchild "BassVoice"
  }

  \context {
    \Voice
    \name "VocalVoice"
    \alias "Voice"
    \consists "Staff_performer"
    midiInstrument = "voice oohs"
  }
  \context {
    \VocalVoice
    \name "SopranoVoice"
    \alias "VocalVoice"
  }
  \context {
    \VocalVoice
    \name "AltoVoice"
    \alias "VocalVoice"
  }
  \context {
    \VocalVoice
    \name "TenorVoice"
    \alias "VocalVoice"
  }
  \context {
    \VocalVoice
    \name "BassVoice"
    \alias "VocalVoice"
  }
}
