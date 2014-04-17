\version "2.18.0"
% include the edition engraver
\include "definitions.ily"

%%%
% prepare some mods for an edition fullscore

% color the notehead red on the second quarter in the second measure
\editionMod fullscore 2 1/4 my.test.Staff.A \once \override NoteHead #'color = #red
% destroy the slur starting on the second quarter in the first measure
\editionMod fullscore 1 2/4 my.test.Staff.A \shape #'((0 . 0)(0 . 1)(0 . -1)(0 . 0)) Slur

% add a break to several times
\editionMMod fullscore #'((2 1/4)) my.test.Score.A { \bar "" \break } % the empty bar permits break inside measure
% add an annotation in form of a TextScript
\editionMod fullscore 2 0/4 my.test.Voice.A -\markup { \with-color #red "what's that?" }

% just another tweak on several times
\editionMMod fullscore #'((1 1/4)(1 3/4)(2 2/4)) my.test.Staff.A \once \override NoteHead.color = #green

% this places the system, starting with measure 7, absolutely
\editionMod fullscore 7 0/1 my.test.Score.A {
  \break
  \overrideProperty Score.NonMusicalPaperColumn.line-break-system-details #'((Y-offset . 70)(X-offset . 5))
}

\layout {
  \context {
    \Score
    % consist the Score with an edition-engraver to inject breaks
    \consists \editionEngraver my.test
  }
  \context {
    \Voice
    % consist every explicitly or implicitly created voice with an edition-engraver, and inherits its tag-path from the parental staff
    \consists \editionEngraver ##f
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
    %\consists \editionEngraver ##f
    % ... but it is already done by the layout block
  } \relative c'' { c4 bes a( g) f e d c \repeat unfold 20 { bes a c b } }
>>

