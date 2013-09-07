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
    \alias "Staff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"
    \description "predefined template for soprano staff"

    \consists "Ambitus_engraver"
    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
    \dynamicUp
    \tupletUp
  }

  \context {
    \Staff
    \name "AltoStaff"
    \alias "Staff"
    \accepts "AltoVoice"
    \defaultchild "AltoVoice"
    \description "predefined template for alto staff"

    \consists "Ambitus_engraver"
    instrumentName = "Alto"
    shortInstrumentName = "A"
    \clef G
    \dynamicUp
    \tupletUp
  }

  \context {
    \Staff
    \name "TenorStaff"
    \alias "Staff"
    \accepts "TenorVoice"
    \defaultchild "TenorVoice"
    \description "predefined template for tenor staff"

    \consists "Ambitus_engraver"
    instrumentName = "Tenor"
    shortInstrumentName = "T"
    \clef "G_8"
    \dynamicUp
    \tupletUp
  }

  \context {
    \Staff
    \name "BassStaff"
    \alias "Staff"
    \accepts "BassVoice"
    \defaultchild "BassVoice"
    \description "predefined template for bass staff"

    \consists "Ambitus_engraver"
    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
    \dynamicUp
    \tupletUp
  }


  \context {
    \Voice
    \name "SopranoVoice"
    \alias "Voice"
    \description "predefined template for soprano voice"
  }

  \context {
    \Voice
    \name "AltoVoice"
    \alias "Voice"
    \description "predefined template for alto voice"
  }

  \context {
    \Voice
    \name "TenorVoice"
    \alias "Voice"
    \description "predefined template for tenor voice"
  }

  \context {
    \Voice
    \name "BassVoice"
    \alias "Voice"
    \description "predefined template for bass voice"
  }
}


\midi {
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
    \alias "Staff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"
    midiInstrument = "voice oohs"
  }
  \context {
    \Staff
    \name "AltoStaff"
    \alias "Staff"
    \accepts "AltoVoice"
    \defaultchild "AltoVoice"
    midiInstrument = "voice oohs"
  }
  \context {
    \Staff
    \name "TenorStaff"
    \alias "Staff"
    \accepts "TenorVoice"
    \defaultchild "TenorVoice"
    midiInstrument = "voice oohs"
  }
  \context {
    \Staff
    \name "BassStaff"
    \alias "Staff"
    \accepts "BassVoice"
    \defaultchild "BassVoice"
    midiInstrument = "voice oohs"
  }

  \context {
    \Voice
    \name "SopranoVoice"
    \alias "Voice"
  }
  \context {
    \Voice
    \name "AltoVoice"
    \alias "Voice"
  }
  \context {
    \Voice
    \name "TenorVoice"
    \alias "Voice"
  }
  \context {
    \Voice
    \name "BassVoice"
    \alias "Voice"
  }
}
