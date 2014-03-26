\version "2.18.0"

\include "definitions.ily"

\header {
  title = "Fuzzy fast scale gesture"
}

music = {
  \time 3/2
  % This one gives a horizontal spacing issue due to parallel music in the other staff
  \repeat-stems #'(18 . -2) <c'' f''>2
    e'2
  \repeat-stems #'(12 . -2.5) <e'' g'>4
    s4 g'4 r4
  % manually specify the beam's slope
  \once \override Beam.positions = #'(4 . 1)
  \repeat-stems #'(10 . 0) <c'' g>8
    \stemNeutral
    r8 e''2. |
}

ref = { \repeat unfold 24 r8 }

#(ly:set-option 'strokeadjust #t)

\score {
    <<
      \new Staff \music
      \new Staff \ref
    >>
}
