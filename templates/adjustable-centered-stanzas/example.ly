
stanzaII = \markup {
  Supercalifragilisticexpialidocious!
}
stanzaIII = \markup {
  La la
}
stanzaIV = \markup {
  foo
}
stanzaV = \markup {
  bar
}

spaceAfterNumber = \markup \hspace #0.8
% it would be good if this was stretchable.
spaceBetweenStanzas = \markup \vspace #2

% must be included after the variables are defined.
\include "definitions.ily"

\stanzas
