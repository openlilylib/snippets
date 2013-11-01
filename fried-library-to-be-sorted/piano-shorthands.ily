\version "2.17.3"
% /includes/ulLibrary/pianoToolbox/pianoStaffShorthands.ily

%{ Turn cross staff stems on and off

%}

% WARNING! Doesn't work with beamed notes.

CSSOn =
#(define-music-function (parser location length)
   (number?)
   #{
     % stems may overlap the other staff
     \override Stem #'cross-staff = ##t
     % extend the stems to reach the other staff
     \override Stem #'length = #length
     % do not print extra flags
     \override Flag #'stencil = ##f
     % prevent beaming as needed 
     \override Beam #'stencil = ##f
     % hide Tuplets
     \override TupletBracket #'stencil = ##f
     \override TupletNumber #'stencil = ##f
   #})

CSSOnce =
#(define-music-function (parser location length)
   (number?)
   #{
     % stems may overlap the other staff
     \once \override Stem #'cross-staff = ##t
     % extend the stems to reach the other staff
     \once \override Stem #'length = #length
     % do not print extra flags
     \once \override Flag #'stencil = ##f
     % prevent beaming as needed 
     \once \override Beam #'stencil = ##f
     % hide Tuplets
     \once \override TupletBracket #'stencil = ##f
     \once \override TupletNumber #'stencil = ##f
   #})

CSSOff = {
  \revert Flag #'stencil
  \revert Stem #'length
  \revert Stem #'cross-staff
  \revert Stem #'color % only used in draftMode
  \revert Beam #'stencil
  \revert TupletBracket #'stencil
  \revert TupletNumber #'stencil
}


%{ Use an arpeggio bracket to indicate a cross staff chord
   Shorthand to once override the arpeggio bracket
   use in combination with a following \arpeggio command
%}
chordBracket = {
  % As of 2013-10-17, the thickness stuff works only with my custom patch
  % (see branch origin/dev/janek/bracket-thickness).  The number is just
  % a factor by which the staffline thickness will be multiplied.
  \once \override PianoStaff.Arpeggio #'thickness = #1.4
  \once \override PianoStaff.Arpeggio #'stencil = #ly:arpeggio::brew-chord-bracket
}


%TODO: Add switching on and off of crossvoice arpeggios


% Change staff in a PianoStaff
% Either just go up or change voiceXXX immediately
% sometimes we just want to set stem direction, but
% not articulations etc. (see for example op.7-7)

SUp = \change Staff = "up"
SDn = \change Staff = "down"
SSUp = { \SUp \voiceTwo }
SSDn = { \SDn \voiceOne }
SSSUp = { \SUp \stemDown }
SSSDn = { \SDn \stemUp }


% General overrides for a PianoStaff
\layout {
  \context {
    \PianoStaff
    connectArpeggios = ##t
    \accidentalStyle "piano"
  }
}
