\version "2.16.2"

%{
  TODO:
  define other instruments
  define voices
  add other useful commands (eg dynamicUp)
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

    \type "Engraver_group"
    \alias "Staff"
    \accepts "Voice"
    \defaultchild "Voice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef G
    instrumentName = "Soprano"
    shortInstrumentName = "S"
  }

  \context {
    \Staff
    \name "AltoStaff"
    \description "predefined template for alto staff"

    \type "Engraver_group"
    \alias "Staff"
    \accepts "Voice"
    \defaultchild "Voice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef G
    instrumentName = "Alto"
    shortInstrumentName = "A"
  }

  \context {
    \Staff
    \name "TenorStaff"
    \description "predefined template for tenor staff"

    \type "Engraver_group"
    \alias "Staff"
    \accepts "Voice"
    \defaultchild "Voice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef "G_8"
    instrumentName = "Tenor"
    shortInstrumentName = "T"
  }

  \context {
    \Staff
    \name "BassStaff"
    \description "predefined template for bass staff"

    \type "Engraver_group"
    \alias "Staff"
    \accepts "Voice"
    \defaultchild "Voice"

    \consists "Ambitus_engraver"
    midiInstrument = "acoustic grand"

    \clef F
    instrumentName = "Bass"
    shortInstrumentName = "B"
  }
}
