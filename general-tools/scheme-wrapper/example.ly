\version "2.17.96"
\include "parserDefine.ily"
\include "a-list.ily"

% with a simple void-function you can define variables almost anywhere
% this means, you can include music files inside a scope.
\score {
  % parserDefine is not allowed here, because it doesn't return a valid music expression, ...
  <<
    % ... but here
    \parserDefine hansi-mausi 25
    % this might be included from a file here
    \parserDefine myMusic \relative c' { c4 e g b c1 }
    \new Staff \myMusic
  >>
  \layout { }
}

#(display (+ hansi-mausi 42))
#(newline)

% There is more to come, where I make excessive use of nested a-lists (or a-trees?) to parametresize options
% this might be called "[clr|set|add]Option" ???
\clratree opts
\setatree opts staffs.trumpet.name "Trumpet"
% addatree leaves the order of input in the assoc-list for the price of a little bit more computing
\addatree opts staffs.trombone.name "Trombone"
\addatree opts staffs.trombone.clef "bass"
#(display opts)
