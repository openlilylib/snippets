\include "shapeII.ily"

\version "2.17.28"

\paper {
  ragged-right = ##t
  indent = 0
}

\header {
  title = "Improved \shape"
}

\markup \vspace #2
\markup \bold {
  LEFT: LilyPond default \hspace #3
  RIGHT: result after applying "\shape"
}

\markup \vspace #1
\markup "Old syntax works as before:"
\markup\small\typewriter "\shapeII #'((0 . -2)(-1 . 3)(-1 . 0.5)(0 . -2.5))"
\markup \vspace #0.7
\relative c'' {
  d4( d' b g g,8 f' e d c2)
  \shapeII #'((0 . -2)(-1 . 3)(-1 . 0.5)(0 . -2.5)) Slur
  d4( d' b g g,8  f' e d c2)
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Shorthands"
\markup \vspace #1

\markup "If just one offset is specified, it's used on all points:"
\markup\small\typewriter "\shapeII #'((3 . 0))"
{
  d''1 ( f'')
  \shapeII #'((3 . 0)) Slur
  d''1 ( f'')
}

\markup "When two offsets are specified, they are mirrored:"
\markup\small\typewriter "\shapeII #'((0 . 0.5)(0 . 2))"
\markup \vspace #0.2
{
  d''1 ( f'')
  \shapeII #'((0 . 0.5)(0 . 2)) Slur
  d''1 ( f'')
}

\markup { To leave some points unaltered, use \typewriter "()": }
\markup\small\typewriter "\shapeII #'(()()()(3 . 0))"
{
  d''1 ( f'')
  \shapeII #'(()()()(3 . 0)) Slur
  d''1 ( f'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Smart offsets"
\markup \vspace #1
\markup {
  If you use 2-element lists instead of pairs, offsets for
  the right control-points will be flipped horizontally...
}
\markup\small\typewriter "\shapeII #'((2 0))"
{
  d''1 ( g'' f'')
  \shapeII #'((2 0)) Slur
  d''1( g'' f'')
}
\markup\small\typewriter "\shapeII #'((-2 -1.5)(-1 2))"
\markup \vspace #0.5
{
  d''1 ( f'')
  d''1-\shapeII #'((-2 -1.5)(-1 2)) ( f'')
}

\markup { ...and vertical offsets will be flipped for downward slurs: }
\noPageBreak
\markup\small\typewriter "\shapeII #'((0 0)(0.5 2))" \noPageBreak
<<
  {
    d''1 ( f'')
    \shapeII #'((0 0)(0.5 2)) Slur
    d''1 ( f'')
  }
  \\
  {
    e'1 ( g')
    \shapeII #'((0 0)(0.5 2)) Slur
    e'1 ( g')
  }
>>

\markup \justify {
  This works for broken slurs as well. If you specify one
  set of instructions, it will be applied to all siblings:
}
\noPageBreak
\markup \vspace #0
\noPageBreak
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
  \hspace #20
  \column {
    \small\typewriter "\shapeII #'(()(1 2))"
    \vspace #0.5
    \score {
      { d''1-\shapeII #'(()(1 2)) ( f'' \break a'' g'') }
      \layout { }
    }
  }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #0.5
\markup\large\bold "Polar coordinates"
\markup \vspace #1

\markup {
  Use polar coordinates for middle points by prefixing
  values with \typewriter polar (or \typewriter p for short):
}
\markup\small\typewriter "\shapeII #'((0 -2)(polar 88 0.5)(polar 20 0.2)())"
\markup\vspace #0.2
\score {
  {
    e2( d'' b'' d''')
    \shapeII #'((0 -2)(polar 88 0.5)(polar 20 0.2)()) Slur
    e2( d'' b'' d''')
  }
  \layout { }
}

\markup { With polar coords, the same values can be used for different slurs: }
\markup\small\typewriter "\shapeII #'(()(p 30 0.7)(p 90 0.3)())"
\score {
  {
    <>_\markup \tiny "<----------(default)--------->"
    e''2 ( d'') |
    e''4 ( e'' d'' d'' )
    \bar "||"
    <>_\markup \tiny "<---------(tweaked)-------->"
    \shapeII #'(()(p 30 0.7)(p 90 0.3)()) Slur
    e''2 ( d'')  |
    e''4 ( e'' d'' d'' )
  }
  \layout { }
}

\markup\line { S-shaped slurs are easy to achieve: }
\markup\small\typewriter "\shapeII #'(()(p -30 0.5)(p 30 0.5)())"
\score {
  { \shapeII #'(()(p -30 0.5)(p 30 0.5)()) Slur a1 ( g) }
  \layout { }
}

\markup\line { Shorthands work with polar coordinates: }
\markup\small\typewriter "\shape-polar #'(()(p 50 0.35))"
\markup\vspace #0.3
\score {
  {
    d''2-\shapeII #'(()(p 50 0.35)) ( f'' f'' d'')
  }
  \layout { }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Other coordinates"
\markup \vspace #1

\markup {
  Other ways of specifying coordinates may be added -
  for example, absolute coordinates:
}
\markup\small\typewriter "\shapeII #'((a 0 0)(a 1 1)(a 5 1)(a 6 0))"
{
  f''1 ( d'')
  \shapeII #'((a 0 0)(a 1 1)(a 5 1)(a 6 0)) Slur
  f''1 ( d'')
}

\markup { Different ways of specifying coordinates may be mixed: }
\noPageBreak
\markup\small\typewriter "\shapeII #'((a 2.3 0.5)(0 . 2)(p 45 0.4)(1 0))"
\noPageBreak \markup \vspace #0.2 \noPageBreak
{
  c''1 ( d'')
  \shapeII #'((a 2.3 0.5)(0 . 2)(p 45 0.4)(1 0)) Slur
  c''1 ( d'')
}