\version "2.17.15"

#(set-global-staff-size 17)

\include "shape-polar.ily"

\layout {
  ragged-right = ##t
  indent = #0
}

\markup \wordwrap {
  Positions of the middle slur control points may be specified using polar coordinates.
  The syntax is
  \typewriter "((x1off y1off) (angle2 rad2) (angle3 rad3) (x4off y4off))"
}
\markup \wordwrap {
  where \typewriter angle2 is the angle between horizontal line and the line
  connecting 1st and 2nd control-points (in degrees), \typewriter rad2 is the
  distance between 1st and 2nd control-points (measured in fraction of the total
  slur length).  Similarly for \typewriter angle3 and \typewriter rad3.
  \typewriter "(x1off y1off)" is the position of first control-point,
  relative to the notehead to which the slur is attached.  Similarly
  \typewriter "(x4off y4off)".
}

\markup \vspace #0.5
\markup \typewriter "\shape-polar #'((0 -2.5)(88 0.5)(20 0.2)(0 0))"
\markup \vspace #0.1
{
  e2( d'' b'' d''')
  e2-\shape-polar #'((0 -2.5)(88 0.5)(20 0.2)(0 0))( d'' b'' d''')
}

\markup { The same values can be used for differently sized slurs: }
\markup \typewriter "\shape-polar #'((0 0)(30 0.6)(90 0.3)(0 0))"
\markup \vspace #0.2
{
  e''2 ( d'') | e''4 ( e'' d'' d'' )
  e''2-\shape-polar #'((0 0)(30 0.6)(90 0.3)(0 0)) ( d'')
  e''4-\shape-polar #'((0 0)(30 0.6)(90 0.3)(0 0)) ( e'' d'' d'' )
}

\markup { S-shaped slurs are very easy to achieve: }
\markup \typewriter "\shape-polar #'(()(-30 0.5)(30 0.5)())"
{
  a1-\shape-polar #'(()(-30 0.5)(30 0.5)()) ( g)
}

\markup { Shorthands work with shape-polar as well: }
\markup \typewriter "\shape-polar #'(()(50 0.3))"
\markup \vspace #0.2
{
  d''2-\shape-polar #'(()(50 0.3)) ( f'' f'' d'')
}

\markup \justify {
  What's most important, this function is more robust against layout changes
  than ordinary "\shape", and allows to specify more generic slur shapes.
}
\markup \vspace #0.3
\markup \justify {
  Take this example: first two measures are two nearly identical phrases
  (the difference is just one accidental) which get two drastically different
  slur shapes by default. 3rd measure contains the same phrase as the 2nd,
  but with changed spacing - again we get a different default slur.
}
\markup \vspace #0.3
\markup \justify {
  Using ordinary "\shape," one would have to find three completely different
  sets of offsets to achieve a similar slur in all cases.  And what's worse,
  any change in the score may have a “butterfly effect” on the slurs -
  it may change how LilyPond would typeset them by default, making user's
  offset values completely wrong.
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
  With this new function, such mishap is almost impossible.
  Note that just \bold one override gives \bold all slurs correct appearance:
}
\markup \typewriter "\shape-polar #'((0 0.5)(85 0.45)(20 0.2)(0 0.3))"
\markup \vspace #0.5

\new PianoStaff <<
  \new Staff = up \relative d {
    \clef G
    \key e \major
    \time 3/16

    \voiceTwo
    \slurUp

    \shape-polar #'((0 0.5)(85 0.45)(20 0.2)(0 0.3)) Slur
    \SDn \times 2/3 { b32( g' b }
    \SUp \times 2/3 { d g e' }
    \times 2/3 { d b g') }
    |
    \shape-polar #'((0 0.5)(85 0.45)(20 0.2)(0 0.3)) Slur
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
    |
    \newSpacingSection
    \override Score.SpacingSpanner #'common-shortest-duration =
    #(ly:make-moment 1 70)
    \shape-polar #'((0 0.5)(85 0.45)(20 0.2)(0 0.3)) Slur
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
