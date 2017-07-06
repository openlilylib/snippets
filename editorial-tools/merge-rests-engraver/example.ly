\version "2.18.2"

\include "oll-core/package.ily"
\loadModule snippets.editorial-tools.merge-rests-engraver
%\include "definition.ily"

\score
{
  \new Staff
  <<
    \new Voice \relative c'
    {
      \voiceOne
      e4 r e r |
      R1 |
      r2 e |
    }
    \new Voice \relative c'
    {
      \voiceTwo
      r4 c r r |
      R1 |
      r2 r4 c |
    }
  >>
  \layout {
    \context { \Staff \consists #merge-rests-engraver } % merges non-whole rests
    \context { \Staff \consists #merge-mmrests-engraver } % merges whole rests
  }
}

\score
{
  \new Staff \relative c'
  <<
    {
      \compressFullBarRests
      c4 r r2 |
      R1 |
      r2 r4 r8 r16 r32 r |
      R1*3 |
    }
    \\
    {
      c4 r r r |
      R1 |
      r2 r4 r8 r16 r32 r |
      R1*3 |
    }
  >>
  \layout {
    \context { \Staff \consists #merge-rests-engraver }
    \context { \Staff \consists #merge-mmrests-engraver }
  }
}
