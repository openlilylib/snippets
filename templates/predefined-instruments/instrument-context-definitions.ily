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
    \Voice
    \name "SopranoVoice"
    \alias "Voice"
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
    \Voice
    \name "AltoVoice"
    \alias "Voice"
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
    \Voice
    \name "TenorVoice"
    \alias "Voice"
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
    \name "BassVoice"
    \alias "Voice"
  }
}
