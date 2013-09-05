\version "2.17.3"

stanzaII = \markup {
}
stanzaIII = \markup {
}
stanzaIV = \markup {
}
stanzaV = \markup {
}

% it would be good if this was stretchable
spaceBetweenStanzas = \markup \vspace #2
spaceAfterNumber = \markup \hspace #1

\markup {
  \fill-line {
    \scale #'(1 . 1) {
      \null

      % 3 is Lily default
      \override #'(baseline-skip . 3)
      \column {
        \line {
          \bold
          "2."
          \spaceAfterNumber
          \stanzaII
        }
        \spaceBetweenStanzas
        \line {
          \bold
          "3."
          \spaceAfterNumber
          \stanzaIII
        }
        \spaceBetweenStanzas
      }

      \null

      \override #'(baseline-skip . 3)
      \column {
        \line {
          \bold
          "4."
          \spaceAfterNumber
          \stanzaIV
        }
        \spaceBetweenStanzas
        \line {
          \bold
          "5."
          \spaceAfterNumber
          \stanzaV
        }
        \spaceBetweenStanzas
      }

      \null
    }
  }
}
