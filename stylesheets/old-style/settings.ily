\version "2.16.2"

\layout {
  \context {
    \Score
    % old editions often have beams almost totally parallel to the notes.
    \override Beam #'damping = #0.5
    extraNatural = ##t
    % heavier appearance
    \override StaffSymbol #'thickness = #1.2
    \override Beam #'beam-thickness = #0.55
  }
}
