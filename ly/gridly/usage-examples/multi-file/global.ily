\version "2.18.2"

#(ly:set-option 'relative-includes #t)

\gridInit #1 #'("soprano" "alto" "tenore" "basso")

\gridSetStructure #1
\with {
  lyrics = \lyricmode { Ooo }
}
\relative c {
  s1 |
}
