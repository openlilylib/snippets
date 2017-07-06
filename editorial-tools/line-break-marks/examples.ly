\version "2.17.10"

\include "oll-core/package.ily"
\loadModule snippets.editorial-tools.line-break-marks
%\include "definitions.ily"

\header {
  title = "Line Break Marks"
  subtitle = "Print indicators for line breaks in the original score"
}

%{ Usage:
   Enter \ļineBreakMark whereever you want the dashed line to appear
   This can be used at barlines or anywhere between.
%}

\markup \vspace #1
{
  s1*2 \mark \default
  s1*2 \lineBreakMark
  s1
  s2 \lineBreakMark
  s2
  s1*2 \mark \default
  s1
}