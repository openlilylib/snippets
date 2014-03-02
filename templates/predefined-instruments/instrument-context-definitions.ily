\version "2.19.1"

%{
  TODO:
  all the boilerplate code should be rewritten using newInstrument function.

  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably shoul be moved to voice.

  define other instruments
%}


newInstrument =
#(define-scheme-function
  (parser location name parent parentname grouping settings)
  (string? ly:context-def? string? ly:context-def? ly:context-mod?)
  (let ((staffname (string-append name "Staff"))
        (voicename (string-append name "Voice"))
        ;; TODO: should be derived from parent, there shouldn't be a separate arg for this
        (parentstaffname (string-append parentname "Staff"))
        (parentvoicename (string-append parentname "Voice")))
    #{
      \layout {
        \context {
          #grouping
          \accepts #staffname
        }
        \context {
          #parent
          \name #staffname
          \alias #parentstaffname
          \accepts #voicename % is it possible to make it accept Voices of derived instruments?
          \defaultchild #voicename

          #settings
        }
        \context {
          \Voice
          \name #voicename
          \alias #parentvoicename
        }
      }
    #}))

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
    \accepts "VocalVoice"
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
