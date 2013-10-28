\version "2.17.29"

\include "./definitions.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This example shows how <snippet-name> can be used.
% The implementation of the snippet is in the file
% `definitions.ily`.
% Documentation (if any) should be in `README.md`.

hairpinMolto =
\hairpinWithCenteredText \markup { \italic \tiny molto } opening #0

hairpinMore =
\hairpinWithCenteredText poco #CENTER #UP

\layout { ragged-right = ##f }

\relative c' {
  \hairpinMolto
  c2\< c\f
  \hairpinMore
  c2\ppppp\< c\f
  \break
  \hairpinMolto
  c2^\< c\f
  \hairpinMore
  c2\ppppp\< c\f
}

\relative c' {
  \hairpinMolto
  c2\> c\f
  \hairpinMore
  c2\ppppp\> c\f
  \break
  \hairpinMolto
  c2^\> c\f
  \hairpinMore
  c2\ppppp\> c\f
}
