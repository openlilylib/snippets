\version "2.19.16"

\include "editorial-tools/edition-engraver/definitions.ily"

\layout {
  \context {
    \Score
    \consists \editionEngraver score
  }
  \context {
    \Voice
    \consists \editionEngraver ##f
  }

}

\editionMod fullscore
  4 0/4
  score.Score.A
  \mark \default

\addEdition fullscore


\new Staff \with { \consists \editionEngraver mystaff }
{
  \mark \default
  \repeat unfold 36 c'
}

