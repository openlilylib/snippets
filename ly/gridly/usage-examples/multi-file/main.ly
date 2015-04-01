\version "2.18.2"

% See global.ily for module loading and initialization
\include "global.ily"
\useModule gridly.grid-templates

% Workaround for strange bug with \useModule,
% maybe due to the optional argument.
% (we need _something_ Scheme-ish before any LilyPond code)
#(display "")

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
