\version "2.18.2"

\include "openlilylib"
\useLibrary gridly

\gridInit #1 #'("marks" "soprano" "alto" "tenore" "basso")

\gridSetSegmentTemplate #1
\with {
  lyrics = \lyricmode { Ooo }
  music = \relative c {
    s1 |
  }
}
