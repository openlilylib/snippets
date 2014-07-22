\version "2.19.1"

%{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DESCRIPTION

As written in the README (see https://github.com/openlilylib/openlilylib/blob/master/templates/predefined-instruments/),
the goal of this extension is to provide users with predefined contexts
that can be used for creating \score blocks easily (see simple-example.ly
for a demonstration).

Of course, I could have defined all these contexts manually, but there
would be too much boilerplate and code duplication that way.  Just look
at the code needed to define a single new "instrument" (Staff + Voice
contexts, both in \layout and in \midi):

\layout {
  \context {
    \ChoirStaff
    \accepts "SopranoStaff"
  }
  \context {
    \Staff   % "inherit" all settings and overrides from Staff context
    \name "SopranoStaff"
    \alias "Staff"   % make overrides for Staff also work with SopranoStaff
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"
    \description "predefined template for soprano staff"

    \consists "Ambitus_engraver"
    \dynamicUp
    \tupletUp
    instrumentName = "Soprano"
    shortInstrumentName = "S"
    \clef G
  }
  \context {
    % even if there are no specific Voice-level settings for this instrument,
    % we want to create a special Voice-like context so that the user will be
    % able to issue Voice-level overrides for this instrument.
    \Voice
    \name "SopranoVoice"
    \alias "Voice"
    \description "predefined template for soprano voice"
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
    \alias "Staff"
    \accepts "SopranoVoice"
    \defaultchild "SopranoVoice"
  }
  \context {
    \Voice
    \name "SopranoVoice"
    \alias "Voice"
  }
}

This is 45 lines of code, only 6 of which contain actual business logic
for the new instrument.  Therefore, we need a function that will automate
the process of creating new contexts; it should also help with creation
of "context hierarchies".  For example, just as DrumVoice is a context
that inherits settings from Voice, we would like to be able to easily
define a VocalStaff (together with accompanying VocalVoice) together
with Soprano/Alto/etc. Staves(Voices) that would "inherit" from
VocalStaff(Voice).

Ideally, this function should take the following arguments:
- name of the new "instrument"
- name of "instrument" from which we inherit settings - this may be
  an optional argument (with the default being to inherit from Staff
  and Voice contexts respectively)
- name of the group type this instrument belongs to (i'm not really sure about this)
- setting for staff and voice contexts.

TODOs
Right now the function has some quirks (roughly in order of importance):

  1) The function should not require specifying parentstaff and parentvoice
  contexts - they should be inferred from "parentname" argument

  2) The function should handle creating appropriate Midi contexts (when i tried 
  it didn't work... right now they are written manually.)
  
  3) make \addLyrics smarter so that it could be used with e.g. SopranoStaff with
  SopranoVoice (see comment in simple-example.ly)

  4) make it so that all \newInstrument calls could be put in one \layout block.

  Other improvements, refactoring etc. are also welcome.  I expect that the function
  could be written in a much more elegant way.

  (for later)
  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably should be moved to voice.
  (for later)
  define other instruments
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNCTION DEFINITION

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% "INSTRUMENT" DEFINTIONS

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
% Why i cannot put definitions of multiple contexts in one \layout????
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

% ugh, this should be handled by newInstrument function, too.
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
