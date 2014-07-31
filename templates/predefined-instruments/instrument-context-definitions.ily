\version "2.19.10"

\include "context-creating-function.ily"

\newInstrument "Vocal" "default"
\with {
  \consists "Ambitus_engraver"
  instrumentName = "Vocals"
  shortInstrumentName = "Voc."
  \dynamicUp
  \tupletUp
  \remove "Staff_performer"
}
\with {
  \consists "Staff_performer"
  midiInstrument = "voice oohs"
}

\newInstrument "Soprano" "Vocal"
\with {
  instrumentName = "Soprano"
  shortInstrumentName = "S"
  \clef G
}
\with { }

\newInstrument "Alto" "Vocal"
\with {
  instrumentName = "Alto"
  shortInstrumentName = "A"
  \clef G
}
\with { }

\newInstrument "Tenor" "Vocal"
\with {
  instrumentName = "Tenor"
  shortInstrumentName = "T"
    \clef "G_8"
}
\with { }

\newInstrument "Bass" "Vocal"
\with {
    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
}
\with { }
