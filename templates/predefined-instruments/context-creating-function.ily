\version "2.19.10"

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

  1) make \addLyrics smarter so that it could be used with e.g. SopranoStaff with
  SopranoVoice (see comment in simple-example.ly)

  2) remove code duplication; general cleanup.

  (for later)
  think how to handle two voices (eg SA) on one staff
  -> what about ambitus in that case? Probably should be moved to voice.
  (for later)
  define other instruments
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNCTION DEFINITION

% Create a new xxxStaff and xxxVoice contexts with specified settings,
% derived from specified yyyStaff and yyyVoice contexts.
newInstrument =
#(define-scheme-function
  (parser location name parent-name group-name staff-settings voice-settings)
  (string? string? string? ly:context-mod? ly:context-mod?)
  (let* ((staff-name (string-append name "Staff"))
         (voice-name (string-append name "Voice"))
         (parent-name (if (string=? parent-name "default") "" parent-name))
         (parent-staff-name (string-append parent-name "Staff"))
         (parent-voice-name (string-append parent-name "Voice")))
    (ly:parser-define! parser '$defaultlayout
      #{
        \layout {
          \context {
            $(module-ref (current-module) (string->symbol group-name))
            \accepts #staff-name
          }
          \context {
            $(module-ref (current-module) (string->symbol parent-staff-name))
            \name #staff-name
            \alias #parent-staff-name
            % is it possible to make it accept Voices of derived instruments?
            \accepts #voice-name
            \defaultchild #voice-name

            #staff-settings
          }
          \context {
            $(module-ref (current-module) (string->symbol parent-voice-name))
            \name #voice-name
            \alias #parent-voice-name

            #voice-settings
          }
        }
      #})
    ;; UGH! code duplication!
    (ly:parser-define! parser '$defaultmidi
      #{
        \midi {
          \context {
            $(module-ref (current-module) (string->symbol group-name))
            \accepts #staff-name
          }
          \context {
            $(module-ref (current-module) (string->symbol parent-staff-name))
            \name #staff-name
            \alias #parent-staff-name
            % is it possible to make it accept Voices of derived instruments?
            \accepts #voice-name
            \defaultchild #voice-name

            #staff-settings
          }
          \context {
            $(module-ref (current-module) (string->symbol parent-voice-name))
            \name #voice-name
            \alias #parent-voice-name

            #voice-settings
          }
        }
      #})))
