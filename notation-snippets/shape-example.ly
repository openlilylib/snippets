\include "shape.ily"

\paper { ragged-right = ##t }

\markup {
  When just one pair of offsets is specified,
  all control-points are offset by this amount:
}
{
  d''1 ( f'')
  d''1-\shape #'((2 0)) ( f'')
}

\markup {
  When two pairs of offsets are specified,
  the other two control-points use X-symmetricall offsets:
}
{
  d''1 ( f'')
  d''1-\shape #'((-0.5 0)(1 2)) ( f'')
}

\markup {
  Offsets for downward slurs are flipped - the same override
  is used for upward and downward slurs here:
}
{
  d''1 ( f'')
  e'1 ( g')
  d''1-\shape #'(()(1 1)) ( f'')
  e'1-\shape #'(()(1 1)) ( g')
}
