\version "2.18.0"
\include "definitions.ily"

<<
  \new Staff \with {
    % make Staff bigger
    \sizeContext #2
  } \relative c'' { bes4 a c b }
  \new Staff \with {
    % leave size of Staff
    % \sizeContext #0 % no size change
  } \relative c'' { bes4 a c b }
  \new Staff \with {
    % make Staff smaller
    \sizeContext #-2
  } \relative c'' { bes4 a c b }
>>
