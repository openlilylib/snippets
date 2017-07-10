\version "2.16.2"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.interval-brackets
%\include "definitions.ily"

\markup \bold \huge "Interval Brackets"

\relative c' {
  \override Staff.TimeSignature #'stencil = ##f
  \time 32/4
  \intervalBracketsOn
  c1\tone
  d\semiTone
  ees\tone
  f\tone
  g\semiTone
  as\threeSemiTone
  b!\semiTone c
  \bar "|."
  \intervalBracketsOff
}
