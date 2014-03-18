\version "2.18.0"
\include "edition-engraver.ily"

% color the notehead red on the second quarter in the second measure
\editionMod fullscore 2 1/4 my.test.Staff.A \once \override NoteHead #'color = #red
% destroy the slur starting on the second quarter in the first measure
\editionMod fullscore 1 2/4 my.test.Staff.A \shape #'((0 . 0)(0 . 1)(0 . -1)(0 . 0)) Slur

\editionMMod fullscore #'((2 1/4)) my.test.Score.A { \bar "" \break }
\editionMod fullscore 2 0/4 my.test.Voice.A -\markup { \with-color #red "what's that?" }

\editionMMod fullscore #'((1 1/4)(1 3/4)(2 2/4)) my.test.Staff.A \once \override NoteHead.color = #green

\layout {
  \context {
    \Score
    \consists \editionEngraver my.test
  }
}

% edition flightname activated
\addEdition fullscore

\new Staff \with {
  % add edition engraver with id-path #'(my test) to this Staff
  \consists \editionEngraver my.test
} <<
  \new Voice \with {
    % add edition engraver to this voice and inherit id-path from parent context: #'(my test) from parent Staff
    \consists \editionEngraver ##f
  } \relative c'' { c4 bes a( g) f e d c }
>>

