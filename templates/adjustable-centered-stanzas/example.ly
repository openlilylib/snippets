\version "2.19.3"

\include "oll-core/package.ily"
\loadModule snippets.templates.adjustable-centered-stanzas
%\include "definitions.ily"

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

\markup \stanzas-in-columns #1 { \stanzaII \stanzaIII \stanzaIV \stanzaV }

\markup \vspace #2
\markup "Stanzas in two columns:"
\markup \draw-hline
\markup \vspace #2

\markup \stanzas-in-columns #2 { \stanzaII \stanzaIII \stanzaIV \stanzaV }

\markup \vspace #2
\markup "Stanzas in two columns with some settings overridden:"
\markup \draw-hline
\markup \vspace #2

\markup
\override #'(horizontal-spacing . 0.3)
\override #'(vertical-spacing . 0.6)
\override #'(first-number . 1)
\stanzas-in-columns #2 { \stanzaII \stanzaIII \stanzaIV \stanzaV }
