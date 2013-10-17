\version "2.17.15"

#(set-global-staff-size 18)

\include "shape-polar.ily"

\layout {
  ragged-right = ##t
  indent = #0
}

\markup \wordwrap {
  Slur shape may be specified using polar coordinates. The syntax is
  \typewriter "((x1off . y1off) (1-2-angle . 1-2-radius) (3-4-angle . 3-4-radius) (x4off . y4off))"
  where \typewriter "(x1off . y1off)" are offsets relative to the notehead to which the slur is attached.
  Angles are in degrees, radius is normalized: 1 means the distance between
  outer control-points.
}
{
  c'2 ( d') | c'4 ( c' d' d' )
  c'2-\shape-polar #'((0 . 0) (30 . 0.6) (90 . 0.3) (0 . 0)) ( d')
  c'4-\shape-polar #'((0 . 0) (30 . 0.6) (90 . 0.3) (0 . 0)) ( c' d' d' )
}
{
  e2( d'' b'' d''')
  e2-\shape-polar #'((0 . -2.5)(88 . 0.5)(20 . 0.2)(0 . 0))( d'' b'' d''')
}
\markup { shorthands work with shape-polar shape as well: }
{
  d''2-\shape-polar #'(()(50 . 0.3)) ( f'' f'' d'')
}

\markup { S-shaped slurs are trivial to achieve: }
{
  a1-\shape-polar #'((1 . 0) (-50 . 0.5) (50 . 0.5) (-1 . 0)) ( g)
}

\markup { broken slurs: }
\markup \line {
  \score {
    { d''1 ( f'' \break a'' g'') }
    \layout { }
  }
  \hspace #10
  \score {
    {
      d''1-\shape-polar #'(((0 . 0.5) (45 . 0.4) (35 . 0.4) (0 . 1))
                     ((0 . 1)(35 . 0.35)(45 . 0.35)(0 . 0))) ( f'' \break a'' g'')
    }
    \layout { }
  }
}

\markup \justify {
  But what's more important, this function is more robust against lilypond layout changes,
  and allows to write more generic slur shapes.
  Take this example: in two nearly identical phrases (the difference is just one accidental)
  get two drastically different slur shapes by default.  Using ordinary "\shape," one
  would have to find two completely different sets of offsets to achieve a similar slur
  in both cases.  And what's worse, any slight change in the score may have a “butterfly effect”
  on the slurs - it may change how LilyPond would typeset them by default, making user's
  offset values completely wrong.
}

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

  }
  \new Staff = down {
    \clef F
    \key e \major
    \time 3/16

    s16*6
  }
>>

\markup \justify {
  With this new function, such mishap is almost impossible.
  Note that just one override gives both slurs correct appearance:
}

\new PianoStaff <<
  \new Staff = up \relative d {
    \clef G
    \key e \major
    \time 3/16

    \voiceTwo
    \slurUp

    \shape-polar #'((0 . 0.5)(85 . 0.45)(20 . 0.2)(0 . 0.3)) Slur
    \SDn \times 2/3 { b32( g' b }
    \SUp \times 2/3 { d g e' }
    \times 2/3 { d b g') }
    |
    \shape-polar #'((0 . 0.5)(85 . 0.45)(20 . 0.2)(0 . 0.3)) Slur
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }

  }
  \new Staff = down {
    \clef F
    \key e \major
    \time 3/16

    s16*6
  }
>>
