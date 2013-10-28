\version "2.17.29"
% Prior to 2.17.29, \shapeII ... Slur (i.e. the non-tweak syntax)
% will probably affect only the first slur in each example.

\include "shapeII.ily"

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
  the right control-points will be flipped horizontally:
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
\markup\small\typewriter "\shapeII #'((0 -2)(polar 0.5 88)(polar 0.2 20)())"
\markup\vspace #0.2
\score {
  {
    e2( d'' b'' d''')
    \shapeII #'((0 -2)(absolute-polar 0.5 88)(ap 0.2 20)()) Slur
    e2( d'' b'' d''')
  }
  \layout { }
}

\markup { With polar coords, the same values can be used for different slurs: }
\markup\small\typewriter "\shapeII #'(()(p 0.7 30)(p 0.3 90)())"
\score {
  {
    <>_\markup \tiny "<----------(default)--------->"
    e''2 ( d'') |
    e''4 ( e'' d'' d'' )
    \bar "||"
    <>_\markup \tiny "<---------(tweaked)-------->"
    \shapeII #'(()(ap 0.7 30)(ap 0.3 90)()) Slur
    e''2 ( d'')  |
    e''4 ( e'' d'' d'' )
  }
  \layout { }
}

\markup\line { S-shaped slurs are easy to achieve: }
\markup\small\typewriter "\shapeII #'(()(p 0.5 -30)(p 0.5 30)())"
\score {
  { \shapeII #'(()(p 0.5 -30)(ap 0.5 30)()) Slur a1 ( g) }
  \layout { }
}

\markup\line { Shorthands work with polar coordinates: }
\markup\small\typewriter "\shapeII #'(()(ap 0.35 50))"
\markup\vspace #0.3
\score {
  {
    d''2-\shapeII #'(()(ap 0.35 50)) ( f'' f'' d'')
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
\markup\small\typewriter "\shapeII #'((a 2.3 0.5)(0 . 2)(ap 45 0.4)(1 0))"
\noPageBreak \markup \vspace #0.2 \noPageBreak
{
  c''1 ( d'')
  \shapeII #'((a 2.3 0.5)(0 . 2)(ap 0.4 45)(1 0)) Slur
  c''1 ( d'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%relative-polar


\score {
  {
    c''2( d'' f'' d'')
    e2( d'' b'' d''')
    \shapeII #'(()(p 0.3 50)(p 0.3 50)()) Slur
    c''2( d'' f'' d'')
    e2( d'' b'' d''')
  }
  \layout { }
}
\score {
  \inversion b' b'{
    \slurDown
    c''2( d'' f'' d'')
    e2( d'' b'' d''')
    \shapeII #'(()(p 0.3 50)(p 0.3 50)()) Slur
    c''2( d'' f'' d'')
    e2( d'' b'' d''')
  }
  \layout { }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% shape can accumulate

{
  d''1 ( f'')
  \shapeII #'((0 2)) Slur
  d''1 ( f'')
  d''1-\shapeII #'(()(0 2)) ( f'')
}


\score {
  {
    e2( d'' b'' d''')
    \shapeII #'(()(rp 2 20)(rp 1 10)()) Slur
    e2( d'' b'' d''')
  }
  \layout { }
}

\markup \vspace #1
\markup \justify {
  What's most important, this function is more robust against layout changes
  than ordinary "\shape", and allows to specify more generic slur shapes.
}
\markup \vspace #0.5
\markup \justify {
  Take this example: first two measures are two nearly identical phrases
  (the difference is just one accidental) which get two drastically different
  slur shapes by default. 3rd measure contains the same phrase as the 2nd,
  but with changed spacing - again we get a different default slur:
}
\markup \vspace #0.5

SUp = \change Staff = "up"
SDn = \change Staff = "down"

\new PianoStaff <<
  \new Staff = up \relative d {
    \clef G
    \key e \major
    \time 3/16

    \voiceTwo
    \slurUp

    \SDn \times 2/3 { b32( g' b }
    \SUp \times 2/3 { d g e' }
    \times 2/3 { d b g') }
    |
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
    |
    \newSpacingSection
    \override Score.SpacingSpanner #'common-shortest-duration =
    #(ly:make-moment 1 70)
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
  }
  \new Staff = down {
    \clef F
    \key e \major
    \time 3/16

    s16*9
  }
>>

\markup \justify {
  Using ordinary "\shape," one would have to find three completely different
  sets of offsets to achieve a similar slur in all cases.  And what's worse,
  any change in the score may have a “butterfly effect” on the slurs -
  it may change how LilyPond would typeset them by default, making user's
  offset values completely wrong.
}
\markup \vspace #0.5
\markup \justify {
  With this new function, such mishap is very unlikely.
  Note that just \bold one override gives \bold all slurs correct appearance:
}
\markup \typewriter "\shapeII #'((h)(p 0.5 55)(p 0.2 50)(h))"
\markup \vspace #0.5

\new PianoStaff <<
  \new Staff = up \relative d {
    \clef G
    \key e \major
    \time 3/16

    \voiceTwo
    \slurUp

    \shapeII #'((h)(p 0.5 55)(p 0.2 50)(h)) Slur
    \SDn \times 2/3 { b32( g' b }
    \SUp \times 2/3 { d g e' }
    \times 2/3 { d b g') }
    |
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
    |
    \newSpacingSection
    \override Score.SpacingSpanner #'common-shortest-duration =
    #(ly:make-moment 1 70)
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
  }
  \new Staff = down {
    \clef F
    \key e \major
    \time 3/16

    s16*9
  }
>>
