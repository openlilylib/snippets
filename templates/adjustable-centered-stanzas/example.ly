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

\markup \stanzas-in-one-column { \stanzaII \stanzaIII \stanzaIV \stanzaV }

\markup \vspace #2
\markup "Stanzas in two columns:"
\markup \draw-hline
\markup \vspace #2

\markup \stanzas-in-two-columns { \stanzaII \stanzaIII } { \stanzaIV \stanzaV }

\markup \vspace #2
\markup "Stanzas in two columns with some settings overridden:"
\markup \draw-hline
\markup \vspace #2

\markup
\override #'(text-scaling . (0.8 . 1))
\override #'(line-spacing . 0.75)
\override #'(number-hdist . 3)
\override #'(stanza-vdist . 0.5)
\override #'(first-number . 1)
\stanzas-in-two-columns { \stanzaII \stanzaIII } { \stanzaIV \stanzaV }
