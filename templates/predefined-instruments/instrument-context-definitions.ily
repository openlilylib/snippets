\version "2.19.1"

%{
  TODO:
  all the boilerplate code should be rewritten using newInstrument function.

  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably shoul be moved to voice.

  define other instruments
%}

% TODO: parentstaff and parentvoice should be derived from parentname,
% there shouldn't be separate args for this
newInstrument =
#(define-scheme-function
  (parser location name parentstaff parentvoice parentname grouping settings)
  (string? ly:context-def? ly:context-def? string? ly:context-def? ly:context-mod?)
  (let ((staffname (string-append name "Staff"))
        (voicename (string-append name "Voice"))
        (parentstaffname (string-append parentname "Staff"))
        (parentvoicename (string-append parentname "Voice")))
    #{
      \layout {
        \context {
          #grouping
          \accepts #staffname
        }
        \context {
          #parentstaff
          \name #staffname
          \alias #parentstaffname
          \accepts #voicename % is it possible to make it accept Voices of derived instruments?
          \defaultchild #voicename

          #settings
        }
        \context {
          #parentvoice
          \name #voicename
          \alias #parentvoicename
        }
      }
    #}))

\layout {
  \newInstrument "Vocal" \Staff \Voice "" \ChoirStaff \with {
    \consists "Ambitus_engraver"
    instrumentName = "Vocals"
    shortInstrumentName = "Voc."
    \dynamicUp
    \tupletUp
  }
}
% Why i cannot put this in one \layout????
\layout {
  \newInstrument "Soprano" \VocalStaff \VocalVoice "Vocal" \ChoirStaff \with {
    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
  }
}

\layout {

  \context {
    \ChoirStaff
    \accepts "AltoStaff"
    \accepts "TenorStaff"
    \accepts "BassStaff"
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
