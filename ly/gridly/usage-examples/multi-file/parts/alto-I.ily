\version "2.18.2"

#(ly:set-option 'relative-includes #t)
\include "../global.ily"
\include "../../../grid.ily"

\gridPutMusic "alto" #1
\relative c' {
  e1 |
}

\gridTest "alto" #1
