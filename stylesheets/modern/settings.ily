\version "2.16.2"

\layout {
  \context {
    \Score
    % I dislike it, but many modern editions have quite flat beams.
    \override Beam #'damping = #2.5
    extraNatural = ##f
  }
}
