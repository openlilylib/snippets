\version "2.18.2"

\include "openlilylib"
\loadModule "gridly"

\gridInit #1 #'("soprano" "alto" "tenore" "basso")

\gridSetStructure #1
\with {
  lyrics = \lyricmode { Ooo }
}
\relative c {
  s1 |
}
