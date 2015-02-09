\version "2.18.2"

#(ly:set-option 'relative-includes #t)
\include "../global.ily"
\include "../../../grid.ily"

\gridPutMusic "soprano" #1
\relative c'' {
  g1 |
}

\gridTest "soprano" #1
