\version "2.18.2"

#(ly:set-option 'relative-includes #t)
\include "../global.ily"
\include "../../../grid.ily"

\gridPutMusic "tenore" #1
\relative c' {
  \clef "violin_8"
  c1 |
}

\gridTest "tenore" #1
