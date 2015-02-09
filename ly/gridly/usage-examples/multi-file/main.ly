\version "2.18.2"

#(ly:set-option 'relative-includes #t)
\include "../../grid.ily"
\include "../../grid-templates.ily"

\include "global.ily"

\include "parts/soprano-I.ily"
\include "parts/alto-I.ily"
\include "parts/tenore-I.ily"
\include "parts/basso-I.ily"

\gridDisplay

\gridCheck

segments = #'all

\score {
  \SATBChoir \segments

  \layout {}
  \midi {}
}

\rehearsalMidi {\SATBChoir \segments} "soprano"

\rehearsalMidi {\SATBChoir \segments} "alto"

\rehearsalMidi {\SATBChoir \segments} "tenore"

\rehearsalMidi {\SATBChoir \segments} "basso"
