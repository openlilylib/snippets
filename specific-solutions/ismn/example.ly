\version "2.17.97"

\include "oll-core/package.ily"
\loadModule snippets.specific-solutions.ismn
%\include "definitions.ily"

%%%%%%%%%
% Example

% create "M 700337-00-1"
myismn = \createISMN "700337" "00"
#(ly:message "created ismn: ~A" myismn)

% use ismn in markup
\markup \column {
  "Bärenreiter, J.S.B., \"Mass in B Minor\""
  \ismn #"006" #"46415"
  "Musikverlag Jan-Peter Voigt, Thomas Dittmann, \"Geistliche Chormusik\""
  \ismn #"700337" #"00"
}
