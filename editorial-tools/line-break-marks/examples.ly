\version "2.17.10"

\include "definitions.ily"

\header {
  title = "Line Break Marks"
  subtitle = "Print indicators for line breaks in the original score"
}

%{ Usage:
   Enter \ļineBreakMark whereever you want the dashed line to appear
   This can be used at barlines or anywhere between.
%}

{
  s1*2 \mark \default
  s1*2 \lineBreakMark
  s1
  s2 \lineBreakMark
  s2
  s1*2 \mark \default
  s1
}