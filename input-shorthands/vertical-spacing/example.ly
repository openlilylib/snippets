\version "2.16.2"
\include "definitions.ily"

\markup \bold \huge "Vertical Spacing"

\paper {
  % instead of a large construction, these four values
  % define the vertical spacing:
  system-system-spacing = \simplespace 25 2 3 60
  annotate-spacing = ##t  % only for demonstration
}

{ c''1 \break c''1 }
