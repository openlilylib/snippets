\version "2.19.1"

%{
  TODO (roughly in order of importance):

  1) parentstaff and parentvoice should be derived from parentname,
  there shouldn't be separate args for this.

  2) make newInstrument handle creating Midi contexts (when i tried it didn't work...
  right now they are handled separately, which is bad.)
  
  3) make \addLyrics smarter so that it could be used with these predefined instruments
  (see comment in simple-example.ly)

  4) it'd be good if all \newInstrument definitions could be put in one \layout block.

  (for later)
  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably should be moved to voice.
  (for later)
  define other instruments
%}
newInstrument =
#(define-scheme-function
  (parser location name parentstaff parentvoice parentname grouping staffsettings voicesettings)
  (string? ly:context-def? ly:context-def? string? ly:context-def? ly:context-mod? ly:context-mod?)
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

          #staffsettings
        }
        \context {
          #parentvoice
          \name #voicename
          \alias #parentvoicename

          #voicesettings
        }
      }
    #}))

\layout {
  \newInstrument "Vocal" \Staff \Voice "" \ChoirStaff
  \with {
    \consists "Ambitus_engraver"
    instrumentName = "Vocals"
    shortInstrumentName = "Voc."
    \dynamicUp
    \tupletUp
  }
  \with { }
}
% Why i cannot put this in one \layout????
\layout {
  \newInstrument "Soprano" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
  }
  \with { }
}
\layout {
  \newInstrument "Alto" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Alto"
    shortInstrumentName = "A"
    \clef G
  }
  \with { }
}
\layout {
  \newInstrument "Tenor" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Tenor"
    shortInstrumentName = "T"
    \clef "G_8"
  }
  \with { }
}
\layout {
  \newInstrument "Bass" \VocalStaff \VocalVoice "Vocal" \ChoirStaff
  \with {
    instrumentName = "Bass"
    shortInstrumentName = "B"
    \clef F
  }
  \with { }
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
