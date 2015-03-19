\version "2.17.10"

\include "openlilylib"

\useLibrary scholarly

%\registerOption documentation.include-file "scholarly/diplomatic-line-breaks.ily"
%\loadModule "_internal/doc-include/usage-example.ily"

\useModule scholarly.diplomatic-line-breaks

% The following is necessary because leaving it out would give lots of (strange) syntax errors
#(ly:message "loaded")

\markup \vspace #1
{
  s1*2 \mark \default
  s1*2
  \diplomaticLineBreak
  s1
  s2.
    \diplomaticLineBreak
    s4
  s1*2 \mark \default
  s1
}