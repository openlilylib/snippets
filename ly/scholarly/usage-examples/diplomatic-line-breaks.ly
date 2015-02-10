\version "2.17.10"

\include "openlilylib"
\registerOption documentation.include-file "scholarly/diplomatic-line-breaks.ily"
\loadModule "_internal/doc-include/usage-example.ily"

\markup \vspace #1
{
  s1*2 \mark \default
  s1*2
  \diplomaticLineBreak
  s1
  s2.
    \diplomaticLineBreak
    s4
  s1*2 \mark \default
  s1
}