\version "2.16.2"

\include "./definitions.ily"

\layout {
  \context {
    \Voice
    \printGrobNames #debug-grob-name-groblist
  }
}

{
  a'4~ a'\fermata g'( f')
}
