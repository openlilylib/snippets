\version "2.17.26"

\include "definitions.ily"


example = \relative c' {
  \key f \major
  c4-.\prallmordent dis->\upbow es--\downbow f!-_\ff |
  g2\trill-!\segno a\turn |
  b1\fermata
}

{
  \bravuraOn
  \example
}

{
  \example
}