\version "2.17.96"
\include "definitions.ily"

% with a simple void-function you can define variables almost anywhere
% this means, you can include music files inside a scope.
\score {
  % parserDefine is not allowed here, because it doesn't return a valid music expression, ...
  <<
    % ... but here
    \parserDefine hansi-mausi 25
    % this might be included from a file here
    \parserDefine myMusic \relative c' { c4 e g b c1 }
    % use the former declared variable
    \new Staff \myMusic
  >>
  \layout { }
}

#(display (+ hansi-mausi 42))
