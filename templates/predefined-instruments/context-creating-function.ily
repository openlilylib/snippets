\version "2.19.10"

%{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DESCRIPTION

As written in the README (see https://github.com/openlilylib/openlilylib/blob/master/templates/predefined-instruments/),
the goal of this extension is to provide users with predefined contexts
that can be used for creating \score blocks easily (see simple-example.ly
for a demonstration).

Since defining such contexts manually is a *LOT* of work (30-40 lines of
boilerplate for each instrument), below is a function that automates the
process of creating new contexts.

This function allows to add new contexts into a sort of hierarchy:
just as DrumStaff and DrumVoice are contexts that inherit settings from
Staff and Voice, we can define e.g. a VocalStaff (with accompanying
VocalVoice), Soprano/Alto/... Staves (and Voices) inheriting from
VocalStaff(Voice), etc.


TODO:
1) make \addLyrics smarter so that it could be used with these custom contexts
   (see overriding-example.ly)
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
  (parser location name staff-settings voice-settings parent-name)
  (string?
   (ly:context-mod? #{ \with {} #})
   (ly:context-mod? #{ \with {} #})
   string?)
  (let* ((staff-name (string-append name "Staff"))
         (voice-name (string-append name "Voice"))
         (parent-name (if (string=? parent-name "default") "" parent-name))
         (parent-staff-name (string-append parent-name "Staff"))
         (parent-voice-name (string-append parent-name "Voice")))
    (ly:parser-define! parser '$defaultlayout
      #{
        \layout {
          \context {
            % I don't know why, but this code makes ChoirStaffs accept new
            % instrument as well.  See this question asked on the mailing list:
            % http://lists.gnu.org/archive/html/bug-lilypond/2014-07/msg00129.html
            \StaffGroup
            \accepts #staff-name
          }
          \context {
            \ChoirStaff
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
            \StaffGroup
            \accepts #staff-name
          }
          \context {
            \ChoirStaff
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
