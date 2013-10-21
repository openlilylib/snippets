
\include "shape-polar.ily"
%\include "shapeII.ily"
%{
\score {
  {
    e2( d''_\markup \tiny "(default)" b'' d''')
    \bar "||"
    e2-\shape-polar #'((0 -2.5)(88 0.5)(20 0.2)(0 0)) (
    d''_\markup \tiny "(tweaked)" b'' d''')
  }
  \layout { }
}
%}
{
  \override Slur positions = #'(-9 . -7)
  c'2( e')
}
{
  c'2( e')
}
{
  \override Slur positions = #'(-3.5 . -2)
  c'2( e')
}