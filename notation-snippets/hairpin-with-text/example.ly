\version "2.17.29"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.hairpin-with-text
%\include "./definitions.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This example shows how <snippet-name> can be used.
% The implementation of the snippet is in the file
% `definitions.ily`.
% Documentation (if any) should be in `README.md`.

\layout {
  indent = 0
  ragged-right = ##f
}

\header {
  title = "Hairpins with text"
}

\markup \vspace #1
\markup {
  You can set horizontal alignmnent of the text to "#LEFT", "#RIGHT",
  "#CENTER" or some numerical value:
}
\relative c' {
  \hairpinWithText poco #LEFT #DOWN
  c2\ppppp\< c\f
  \hairpinWithText poco #0.6 #DOWN
  c2\ppppp\< c\f
}

\markup \vspace #0.5
\markup {
  Use special value \typewriter\small "opening"
  to always place the text on the side where the mouth of the hairpin is:
}
\markup\typewriter\small "\hairpinWithText poco opening #DOWN"
\relative c' {
  \hairpinWithText poco opening #DOWN
  c2\ppppp\< c\f
  \hairpinWithText poco opening #DOWN
  c2\ppppp\> c\f
}

\markup \vspace #0.5
\markup {
  You can set vertical alignmnent of the text to "#UP" or "#DOWN":
}
\relative c' {
  \hairpinWithText poco #CENTER #UP
  c2\ppppp\< c\f
  \hairpinWithText poco #CENTER #DOWN
  c2\ppppp\< c\f
}

\markup \vspace #0.5
\markup {
  When the vertical direction of the hairpin changes, the direction
  of the text will change accordingly:
}
\relative c' {
  \hairpinWithText poco #CENTER #DOWN
  c2^\ppppp^\< c\f
  \hairpinWithText poco #CENTER #DOWN
  c2\ppppp\< c\f
}

\markup \vspace #0.5
\markup {
  You can use markups:
}
\relative c' {
  \hairpinWithText \markup\italic { poco a \bold poco } #LEFT #DOWN
  c2\ppppp\< c\f
  \hairpinWithText \markup\rotate #-90 "trololo!" #CENTER #DOWN
  c2\ppppp\< c\f
}


\markup \vspace #0.5
\markup {
  You can put the text inside the hairpin; its opening will be automatically widened.
  Note that Elaine Gould explicitly recommends against this practice.
}
\relative c' {
  \hairpinWithText \markup { \italic \tiny molto } opening #CENTER
  c2\ppppp\< c\f
  \hairpinWithText \markup { \italic \tiny molto } opening #CENTER
  c2\ppppp\> c\f
}
