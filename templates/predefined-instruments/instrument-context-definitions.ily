\version "2.19.10"

\include "context-creating-function.ily"

\layout {
  \newLayoutInstrument "Vocal" \Staff \Voice "" \ChoirStaff
  \with {
    \consists "Ambitus_engraver"
    instrumentName = "Vocals"
    shortInstrumentName = "Voc."
    \dynamicUp
    \tupletUp
  }
  \with { }
}
\midi {
  \newMidiInstrument "Vocal" \Staff \Voice "" \ChoirStaff
  \with {
    \remove "Staff_performer"
  }
  \with {
    \consists "Staff_performer"
    midiInstrument = "voice oohs"
  }
}

\layout {
  \newLayoutInstrument "Soprano" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
  }
  \with { }
}
\midi {
  \newMidiInstrument "Soprano" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with { }
  \with { }
}

\layout {
  \newLayoutInstrument "Alto" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Alto"
    shortInstrumentName = "A"
    \clef G
  }
  \with { }
}
\midi {
  \newMidiInstrument "Alto" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with { }
  \with { }
}

\layout {
  \newLayoutInstrument "Tenor" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Tenor"
    shortInstrumentName = "T"
    \clef "G_8"
  }
  \with { }
}
\midi {
  \newMidiInstrument "Tenor" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with { }
  \with { }
}

\layout {
  \newLayoutInstrument "Bass" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
  }
  \with { }
}
\midi {
  \newMidiInstrument "Bass" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with { }
  \with { }
}
