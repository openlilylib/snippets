\version "2.18.2"

#(ly:set-option 'relative-includes #t)
\include "../global.ily"
\include "../../../grid.ily"

\gridPutMusic "basso" #1
\relative c {
  \clef "bass"
  c1 |
  \bar "|."
}

\gridTest "basso" #1


