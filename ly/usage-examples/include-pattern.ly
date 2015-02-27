\version "2.18.0"

\include "openlilylib"

% show all includes
includePatternVerbose = ##t

% include all files with pattern (regular expression)
% the file f1.ly to f3.ly simply display a string
\includePattern "include-pattern-examples" ".*\\.i?ly"
