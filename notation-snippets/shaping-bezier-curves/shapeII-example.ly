\include "shapeII.ily"

\version "2.17.28"

\paper {
  ragged-right = ##t
  indent = 0
}

\header {
  title = "Better \shape: input shorthands"
}

\markup \vspace #3
\markup {
  When just one pair of offsets is specified,
  all control-points are offset by this amount:
}
\markup \typewriter "\shapeII #'((3 0))"
{
  d''1 ( f'')
  d''1-\shapeII #'((3 0)) ( f'')
}

\markup {
  When two pairs of offsets are specified,
  the other two control-points use X-symmetricall offsets:
}
\markup \typewriter "\shapeII #'((-2 -1.5)(-1 2))"
\markup \vspace #0.5
{
  d''1 ( f'')
  d''1-\shapeII #'((-2 -1.5)(-1 2)) ( f'')
}

\markup {
  Offsets for downward slurs are flipped - the same override
  is used for upward and downward slurs here:
}
\markup \typewriter "\shapeII #'((0 0)(1 2))"
{
  d''1 ( f'')
  e'1 ( g')
  d''1-\shapeII #'((0 0)(1 2)) ( f'')
  e'1-\shapeII #'((0 0)(1 2)) ( g')
}

\markup {
  () is a shorthand for (0 0):
}
\markup \typewriter "\shapeII #'(()(0 3)(-3 -3)())"
\markup \vspace #0.5
{
  d''1 ( f'')
  d''1-\shapeII #'(()(0 3)(-3 -3)()) ( f'')
}

\markup {
  All this works for broken slurs as well:
}
\markup \vspace #0.5
\markup \line {
  \column {
    \vspace #0.2
    default:
    \vspace #0.5
    \score {
      { d''1 ( f'' \break a'' g'') }
      \layout { }
    }
  }
  \hspace #10
  \column {
    \typewriter "\shapeII #'(() (()(0.5 2)))"
    \vspace #0.5
    \score {
      { d''1-\shapeII #'(() (()(0.5 2))) ( f'' \break a'' g'') }
      \layout { }
    }
  }
}
