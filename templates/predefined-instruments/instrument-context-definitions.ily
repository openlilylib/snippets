\version "2.16.2"

%{
  TODO:
  define other instruments
  derive all vocal staves from one VocalStaff
  think how to handle midi instruments
  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case?
%}

\layout {
  \context {
    \ChoirStaff
    \accepts "SopranoStaff"
    \accepts "AltoStaff"
    \accepts "TenorStaff"
    \accepts "BassStaff"
  }

  \context {
    \Staff
    \name "SopranoStaff"
    \description "predefined template for soprano staff"

    \alias "Staff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef G
    \dynamicUp
    \tupletUp
    instrumentName = "Soprano"
    shortInstrumentName = "S"
  }

  \context {
    \Voice
    \name "SopranoVoice"
    \description "predefined template for soprano voice"

    \alias "Voice"
  }

  \context {
    \Staff
    \name "AltoStaff"
    \description "predefined template for alto staff"

    \alias "Staff"
    \accepts "AltoVoice"
    \defaultchild "AltoVoice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef G
    \dynamicUp
    \tupletUp
    instrumentName = "Alto"
    shortInstrumentName = "A"
  }

  \context {
    \Voice
    \name "AltoVoice"
    \description "predefined template for alto voice"

    \alias "Voice"
  }

  \context {
    \Staff
    \name "TenorStaff"
    \description "predefined template for tenor staff"

    \alias "Staff"
    \accepts "TenorVoice"
    \defaultchild "TenorVoice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef "G_8"
    \dynamicUp
    \tupletUp
    instrumentName = "Tenor"
    shortInstrumentName = "T"
  }

  \context {
    \Voice
    \name "TenorVoice"
    \description "predefined template for tenor voice"

    \alias "Voice"
  }

  \context {
    \Staff
    \name "BassStaff"
    \description "predefined template for bass staff"

    \alias "Staff"
    \accepts "BassVoice"
    \defaultchild "BassVoice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef F
    \dynamicUp
    \tupletUp
    instrumentName = "Bass"
    shortInstrumentName = "B"
  }

  \context {
    \Voice
    \name "BassVoice"
    \description "predefined template for bass voice"

    \alias "Voice"
  }

}


\midi {
  \context {
    \ChoirStaff
    \accepts "SopranoStaff"
  }

  \context {
    \Staff
    \name "SopranoStaff"
    \description "predefined template for soprano staff"

    \alias "Staff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"

    midiInstrument = "acoustic grand"
  }

  \context {
    \Voice
    \name "SopranoVoice"
    \description "predefined template for soprano voice"

    \alias "Voice"
    midiInstrument = "acoustic grand"
  }
}
