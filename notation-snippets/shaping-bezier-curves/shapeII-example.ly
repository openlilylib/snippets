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
  LEFT: default LilyPond curve \hspace #3
  RIGHT: result after applying "\shape"
}

\markup \vspace #1.5
\markup \justify {
  Old "\shape" syntax works as before - you can specify offset
  relative to default control-points placement:
}
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
  control-points on the right will be flipped horizontally:
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #0.5
\markup\large\bold "Polar coordinates"
\markup \vspace #1

\markup \justify {
  You can use polar coordinates to specify control-points' positions
  (this is especially useful for middle points). Use the following syntax:
  \small\typewriter { (polar \concat { \italic "radius angle" ) } }
  (Instead of “polar” you can use “p” to save typing).
  Angle is measured in degrees, relative to the line connecting slur ends.
  The unit is which radius is measured is the distance between slur ends.
}
\markup\vspace #0.2
\markup\small\typewriter "\shapeII #'(()(polar 0.5 60)()())"
\markup\vspace #0.2
{
  e''1( e'')
  \shapeII #'(()(polar 0.5 60)()()) Slur
  e''1( e'')
}

\markup {
  With polar coords, the same values can be used for different slurs
  to produce very similar shapes:
}
\markup\small\typewriter "\shapeII #'(()(p 0.7 30)(p 0.3 90)())"

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

\markup \justify {
  Here's an example showing how the reference direction
  depends on the positions of slur ends:
}
\markup\small\typewriter"\shapeII #'(()(polar 0.5 60))"
{
  b'1( b')
  d'( g'')
  \shapeII #'(()(polar 0.5 60)) Slur
  b'1( b')
  d'( g'')
}

\markup \justify {
  You can also specify “absolute” angle (measured against horizontal direction):
}
\markup\small\typewriter "\shapeII #'((0 -2)(absolute-polar 0.5 88)(ap 0.2 20)())"
\markup\vspace #0.2
\score {
  {
    g2( d'' b'' d''')
    \shapeII #'((0 -2)(absolute-polar 0.5 88)(ap 0.2 20)()) Slur
    g2( d'' b'' d''')
  }
  \layout { }
}

\markup \justify {
  You can specify a point's polar coordinates relative to its default
  polar coordinates. For example, this command will place 2nd point
  two times farther and 20 degrees more outwards than default placement:
}
\noPageBreak
\markup\small\typewriter "\shapeII #'(()(rp 2 20)()()) Slur"
\noPageBreak
\score {
  {
    b2( d'' a'' b'')
    \shapeII #'(()(rp 2 20)()()) Slur
    b2( d'' a'' b'')
  }
  \layout { }
}

\markup\line { S-shaped slurs are easy to achieve: }
\markup\small\typewriter "\shapeII #'(()(p 0.5 -30)(p 0.5 30)())"
\score {
  { \shapeII #'(()(p 0.5 -30)(p 0.5 30)()) Slur a1 ( g) }
  \layout { }
}

\markup\line { Shorthands work with polar coordinates: }
\noPageBreak
\markup\small\typewriter "\shapeII #'(()(p 0.35 50))"
\noPageBreak \markup\vspace #0.3 \noPageBreak
\score {
  {
    d''2-\shapeII #'(()(p 0.35 50)) ( f'' f'' d'')
  }
  \layout { }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Other instructions"
\noPageBreak
\markup \vspace #1
\noPageBreak
\markup {
  Other ways of specifying coordinates may be added -
  for example, absolute coordinates:
}
\noPageBreak
\markup\small\typewriter "\shapeII #'((a 0 0)(a 1 1)(a 5 1)(a 6 0))"
\noPageBreak
{
  f''1 ( d'')
  \shapeII #'((a 0 0)(a 1 1)(a 5 1)(a 6 0)) Slur
  f''1 ( d'')
}

\markup \justify  {
  You can position slur ends relative to respective noteheads.
  By default, “head” instruction will place the point so that
  it will be horizontally centered on the notehead, and vertically
  it will be about 0.7 ss away from the notehead:
}
\markup\small\typewriter "\shapeII #'((h)(-1 . 3)(-3 . 0)(h))"
\relative c'' {
  d4( d' b g g,8 f' e d c2)
  \shapeII #'((h)(-1 . 3)(-3 . 0)(h)) Slur
  d4( d' b g g,8  f' e d c2)
}

\markup \justify  {
  You can also specify exact offsets with the “head” instruction.
  They will be measured relative to head center.
}
\markup\small\typewriter "\shapeII #'((head 0 1.3)(p 0.3 -35)()(0 -0.2))"
{
  c''1 ( c'')
  \shapeII #'((head 0 1.3)(p 0.3 -35)()(0 -0.2)) Slur
  c''1 ( c'')
}

\markup { As you can see, different ways of specifying coordinates may be mixed: }
\noPageBreak
\markup\small\typewriter "\shapeII #'((a 2.3 0.5)(0 . 2)(ap 45 0.4)(1 0))"
\noPageBreak \markup \vspace #0.2 \noPageBreak
{
  c''1 ( d'')
  \shapeII #'((a 2.3 0.5)(0 . 2)(ap 0.4 45)(1 0)) Slur
  c''1 ( d'')
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\noPageBreak
\markup\large\bold "Accumulation"
\noPageBreak
\markup \vspace #1
\noPageBreak
\markup \justify {
  If you use "\shape" both as an "\override" and as a "\\tweak", you can
  accumulate its effects.  In the example below, firstly all subsequent
  slurs are moved upwards, and then one of them is given more curvature:
}
\markup \typewriter \column {
  "{"
  "  a'1 ( c'')"
  "  \shapeII #'((0 . 1.5)) Slur"
  "  a'1 ( c'')"
  "  a'1-\shapeII #'(()(rp 1.5 20)) ( c'')"
  "}"
}
{
  a'1 ( c'')
  \shapeII #'((0 . 1.5)) Slur
  a'1 ( c'')
  a'1-\shapeII #'(()(rp 1.5 20)) ( c'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Broken curves"
\noPageBreak
\markup \vspace #1
\noPageBreak
\markup \column {
  "All of this should work for broken slurs/ties as well."
  "If you specify one set of instructions, it will be applied to all siblings:"
}
\noPageBreak
\markup \vspace #0
\noPageBreak
\markup \line {
  \column {
    \vspace #0.5
    default:
    \vspace #0.5
    \score {
      { d''1 ( f'' \break a'' g'') }
      \layout { }
    }
  }
  \hspace #20
  \column {
    \vspace #0.5
    \small\typewriter "\shapeII #'(()(rp 2 15))"
    \vspace #0.5
    \score {
      { d''1-\shapeII #'(()(rp 2 15)) ( f'' \break a'' g'') }
      \layout { }
    }
  }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Why care about all these options?"
\noPageBreak
\markup \vspace #1
\markup \justify {
  This function - and polar coordinates in particular -
  is more robust against layout changes than ordinary "\shape",
  and allows to specify more generic slur shapes.
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
  Such misfortune is unlikely with polar coords.
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

