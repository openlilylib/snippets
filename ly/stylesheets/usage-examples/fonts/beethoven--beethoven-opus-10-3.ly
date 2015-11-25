\version "2.19.12"
\language "english"

\include "openlilylib"
\useLibrary stylesheets
\useNotationFont Beethoven

%TODO: This file includes an ugly mix of Henle stylesheet snippets and a few general
% tools like "bars-per-line-systems-per-page-engraver" that should find their proper
% place in openLilyLib.
\include "beethoven--example-helpers.ily"

% Include the actual music
% TODO: Move the tweaks to edition-engraver
\include "beethoven--notes.ily"


\paper {
  page-count = 1
  systems-per-page = 6
}


\bookpart {

  \header {
    title = \markup \override #'(word-space . 1) \line { S O N A T E }
    opus = "Opus 10 Nr. 3"
    dedication = "Der Gräfin Anna Margarete von Browne gewidmet"
    date = "1796/98"
  }

  \score {
    \new PianoStaff \with {
      instrumentName = "7."
    }
    <<
      \new Staff << \OpXNoIII_piano_global \OpXNoIII_piano_notes_upper >>
      \new Dynamics \OpXNoIII_piano_dynamics
      \new Staff << \OpXNoIII_piano_global \OpXNoIII_piano_notes_lower >>
    >>
    \layout {
      \context {
        \Score
        \consists #(bars-per-line-systems-per-page-engraver '((6*2 5*4)))
        \override NonMusicalPaperColumn.line-break-permission = ##f
        \override NonMusicalPaperColumn.page-break-permission = ##f
      }
    }
  }
}