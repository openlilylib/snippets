\version "2.19.1"

%{
  TODO:
  newInstrument function should be finished (see below), and all the boilerplate code
  should be rewritten using this function.

  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably shoul be moved to voice.

  define other instruments
%}


%{
  Note that this version is very much unfinished - in particular it should be possible
  to specify from which context the newly created instrument should "inherit" properties.
%}
newInstrument =
#(define-scheme-function (parser location name parent grouping settings)
   (string? ly:context-def? ly:context-def? ly:music?)
   (let ((staffname (string-append name "Staff"))
         (voicename (string-append name "Voice")))
       #{
         \layout {
           \context {
             #grouping
             \accepts #staffname
           }
           \context {
             #parent
             \name #staffname
             \alias "Staff"   % TODO this should be derived from "parent"
             \accepts #voicename
             \defaultchild #voicename

             #settings
           }
           \context {
             \Voice
             \name #voicename
             \alias "Voice"   % TODO this should be derived from "parent"
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
