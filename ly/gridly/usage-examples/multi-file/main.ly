\version "2.18.2"

\include "global.ily"
\loadModule "gridly/grid-templates.ily"

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
