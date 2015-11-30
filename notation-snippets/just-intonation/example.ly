\version "2.19.32"

\include "definitions.ily"

\paper {
  score-markup-spacing.minimum-distance = 15
}

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

\markup { Idealized harmonic series through the first eight partials }

{
  c1 c' g' c'' e'' g'' bes'' c'''
}

\markup { Real harmonic series }

{
  \ji c1 1/1
  \ji c 2/1
  \ji c 3/1
  \ji c 4/1
  \ji c 5/1
  \ji c 6/1
  \ji c 7/1
  \ji c 8/1
}

\markup { “Logarithmic” scale approaching the fundamental }

{
  \ji c'1 2/1
  \ji c' 3/2
  \ji c' 4/3
  \ji c' 5/4
  \ji c' 6/5
  \ji c' 7/6
  \ji c' 8/7
  \ji c' 9/8
  \ji c' 10/9
  \ji c' 11/10
  \ji c' 12/11
}


\markup { Achieve \italic nearly the same result with different fundamentals }

{
  \ji d'1 2/1
  \ji g 3/1
  \ji d 4/1
  \ji bes, 5/1
  \ji e, 7/1
  \ji c, 9/1
  \ji bes,, 10/1
  \ji as,, 11/1
}
