\version "2.19.10"

\include "context-creating-function.ily"

\newInstrument "Vocal"
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
"default"

\newInstrument "Soprano"
\with {
  instrumentName = "Soprano"
  shortInstrumentName = "S"
  \clef G
}
"Vocal"

\newInstrument "Alto"
\with {
  instrumentName = "Alto"
  shortInstrumentName = "A"
  \clef G
}
"Vocal"

\newInstrument "Tenor"
\with {
  instrumentName = "Tenor"
  shortInstrumentName = "T"
  \clef "G_8"
}
"Vocal"

\newInstrument "Bass"
\with {
  instrumentName = "Bass"
  shortInstrumentName = "B"
  \clef F
}
"Vocal"
