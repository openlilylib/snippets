\version "2.19.3"

\include "definitions.ily"

stanzaII = \markup \column {
  Supercalifragilisticexpialidocious!
  Supercalifragilisticexpialidocious!
}
stanzaIII = \markup \column{
  "La la la laaaaaa"
  "pam pam pam paaam"
}
stanzaIV = \markup \column {
  foo
  foo
  foo
}
stanzaV = \markup {
  bar
}

\markup "Stanzas in a single column:"
\markup \draw-hline
\markup \vspace #2

\markup \stanzas { \stanzaII \stanzaIII \stanzaIV \stanzaV }

\markup \vspace #2
\markup "Stanzas in two columns:"
\markup \draw-hline
\markup \vspace #2

\markup
\override #'(column-count . 2)
\stanzas { \stanzaII \stanzaIII \stanzaIV \stanzaV }

\markup \vspace #2
\markup "Stanzas in two columns with some settings overridden:"
\markup \draw-hline
\markup \vspace #2

\markup
\override #'(horizontal-spacing . 0.3)
\override #'(vertical-spacing . 0.6)
\override #'(first-number . 1)
\override #'(column-count . 2)
\stanzas { \stanzaII \stanzaIII \stanzaIV \stanzaV }
