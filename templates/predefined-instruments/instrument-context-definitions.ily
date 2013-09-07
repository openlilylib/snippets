\version "2.16.2"

%{
  TODO:
  There's a lot of boilerplate code here.  This should be done
  using some function, and i'm pretty sure that such a function
  would be quite easy to write...

  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably shoul be moved to voice.

  define other instruments
%}

\layout {

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
    \accepts "VocalVoice" % it should perhaps also accept derived Voices
    \defaultchild "VocalVoice"
    \description "predefined template for vocal staves"

    \consists "Ambitus_engraver"
    instrumentName = "Vocals"
    shortInstrumentName = "Voc."
    \dynamicUp
    \tupletUp
  }

  \context {
    \VocalStaff
    \name "SopranoStaff"
    \alias "VocalStaff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"
    \description "predefined template for soprano staff"

    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
  }

  \context {
    \VocalStaff
    \name "AltoStaff"
    \alias "VocalStaff"
    \accepts "AltoVoice"
    \defaultchild "AltoVoice"
    \description "predefined template for alto staff"

    instrumentName = "Alto"
    shortInstrumentName = "A"
    \clef G
  }

  \context {
    \VocalStaff
    \name "TenorStaff"
    \alias "VocalStaff"
    \accepts "TenorVoice"
    \defaultchild "TenorVoice"
    \description "predefined template for tenor staff"

    instrumentName = "Tenor"
    shortInstrumentName = "T"
    \clef "G_8"
  }

  \context {
    \VocalStaff
    \name "BassStaff"
    \alias "VocalStaff"
    \accepts "BassVoice"
    \defaultchild "BassVoice"
    \description "predefined template for bass staff"

    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
  }


  \context {
    \Voice
    \name "VocalVoice"
    \alias "Voice"
    \description "predefined template for vocal voice"
  }

  \context {
    \VocalVoice
    \name "SopranoVoice"
    \alias "VocalVoice"
    \description "predefined template for soprano voice"
  }

  \context {
    \VocalVoice
    \name "AltoVoice"
    \alias "VocalVoice"
    \description "predefined template for alto voice"
  }

  \context {
    \VocalVoice
    \name "TenorVoice"
    \alias "VocalVoice"
    \description "predefined template for tenor voice"
  }

  \context {
    \VocalVoice
    \name "BassVoice"
    \alias "VocalVoice"
    \description "predefined template for bass voice"
  }
}


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
