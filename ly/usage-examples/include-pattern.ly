\version "2.18.0"

\include "openlilylib"

% show all includes
\setOption global.loglevel #oll-loglevel-log
\setOption internal.include-pattern.display-includes ##t

% include all files with pattern (regular expression)
% the file f1.ly to f3.ly simply display a string
% Arguments:
% #1: directory, relative to the location of the file where it is used
% #2: pattern matching files in that directory
\includePattern "include-pattern-examples" ".*\\.i?ly"
