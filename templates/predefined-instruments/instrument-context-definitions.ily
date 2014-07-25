\version "2.19.10"

\include "context-creating-function.ily"

\newInstrument "Vocal" "default" "ChoirStaff"
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

\newInstrument "Soprano" "Vocal" "ChoirStaff"
\with {
  instrumentName = "Soprano"
  shortInstrumentName = "S"
  \clef G
}
\with { }

\newInstrument "Alto" "Vocal" "ChoirStaff"
\with {
  instrumentName = "Alto"
  shortInstrumentName = "A"
  \clef G
}
\with { }

\newInstrument "Tenor" "Vocal" "ChoirStaff"
\with {
  instrumentName = "Tenor"
  shortInstrumentName = "T"
    \clef "G_8"
}
\with { }

\newInstrument "Bass" "Vocal" "ChoirStaff"
\with {
    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
}
\with { }
