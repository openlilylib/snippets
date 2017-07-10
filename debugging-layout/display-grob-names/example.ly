\version "2.16.2"

\include "oll-core/package.ily"
\loadModule snippets.debugging-layout.display-grob-names
%\include "./definitions.ily"

\layout {
  \context {
    \Voice
    \printGrobNames #debug-grob-name-groblist
  }
}

{
  a'4~ a'\fermata g'( f')
}
