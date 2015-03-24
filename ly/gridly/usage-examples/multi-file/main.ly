\version "2.18.2"

%\include "global.ily"


\include "openlilylib"
\useLibrary gridly

\useModule gridly.grid-templates

% Workaround for strange bug with \useModule,
% maybe due to the optional argument.
% (we need _something_ Scheme-ish before any LilyPond code)
#(display "")

\gridInit #1 #'("marks" "soprano" "alto" "tenore" "basso")

\gridSetSegmentTemplate #1
\with {
  lyrics = \lyricmode { Ooo }
}
\relative c {
  s1 |
}

\include "parts/soprano-I.ily"
\include "parts/alto-I.ily"
\include "parts/tenore-I.ily"
\include "parts/basso-I.ily"

\gridDisplay

\gridCheck

\score {
  \SATBChoir

  \layout {}
  \midi {}
}

\rehearsalMidi {\SATBChoir } "soprano"

\rehearsalMidi {\SATBChoir } "alto"

\rehearsalMidi {\SATBChoir } "tenore"

\rehearsalMidi {\SATBChoir } "basso"
