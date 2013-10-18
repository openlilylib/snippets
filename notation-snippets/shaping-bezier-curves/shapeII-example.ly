\include "shapeII.ily"

\version "2.17.28"

\paper { ragged-right = ##t }

\markup {
  When just one pair of offsets is specified,
  all control-points are offset by this amount:
}
{
  d''1 ( f'')
  d''1-\shapeII #'((3 0)) ( f'')
}

\markup {
  When two pairs of offsets are specified,
  the other two control-points use X-symmetricall offsets:
}
{
  d''1 ( f'')
  d''1-\shapeII #'((-2 -1.5)(-1 2)) ( f'')
}

\markup {
  Offsets for downward slurs are flipped - the same override
  is used for upward and downward slurs here:
}
{
  d''1 ( f'')
  e'1 ( g')
  d''1-\shapeII #'((0 0)(1 2)) ( f'')
  e'1-\shapeII #'((0 0)(1 2)) ( g')
}

\markup {
  () is a shorthand for (0 0)
}
{
  d''1 ( f'')
  d''1-\shapeII #'(()(0 3)(-3 -3)()) ( f'')
}

\markup {
  All this works for broken slurs as well:
}
{
  d''1-\shapeII #'(() (()(0.5 2)))( f'' \break a'' g'')
}
