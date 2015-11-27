\version "2.19.32"

\include "definitions.ily"

\layout {
  \context {
    \Voice
    \override TextScript.font-size = #-2
  }
  \context {
    \Staff
    \accidentalStyle dodecaphonic
  }
}

\markup { “Logarithmic” scale approaching the fundamental }

{
  \jiNote 1 2/1
  \jiNote 3/2
  \jiNote 4/3
  \jiNote 5/4
  \jiNote 6/5
  \jiNote 7/6
  \jiNote 8/7
  \jiNote 9/8
  \jiNote 10/9
  \jiNote 11/10
  \jiNote 12/11
}

\markup { Achieve \italic nearly the same result with different fundamentals }

{
  \jiTonic d'
  \jiNote 1 2/1
  \jiTonic g
  \jiNote 3/1
  \jiTonic d
  \jiNote 4/1
  \jiTonic bes,
  \jiNote 5/1
  \jiTonic e,
  \jiNote 7/1
}

