\version "2.18.2"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.interchangeable-metres
%\include "definitions.ily"

\paper {
  bookTitleMarkup = \markup {
    \column {
      \fill-line {
        \fontsize #7 \bold
        \fromproperty #'header:snippet-title
      }
      \vspace #1
      \wordwrap {
        \fromproperty #'header:snippet-description
      }
    }
  }
}


\markup { \vspace #2 \bold { Alternative i: use equal sign } }
\relative c'' {
  \time 6/8
  c4. c8 c c
  \once\override Staff.TimeSignature.stencil =
  \add-interch-ts-eqsg #'(3 4)
  \time 9/8
  c4. c8 c c c4 c8
  \omit Score.TimeSignature
  \time 3/4
  c8 c c c c4
}

\markup { \bold { Alternative ii: use parenthesis } }
\relative c'' {
  \time 6/8
  c4. c8 c c
  \once\override Staff.TimeSignature.stencil =
  \add-interch-ts-prnt #'(3 4)
  \time 9/8
  c4. c8 c c c4 c8
  \omit Score.TimeSignature
  \time 3/4
  c8 c c c c4
}

\markup { \bold { Alternative iii: use dash } }
\relative c'' {
  \time 6/8
  c4. c8 c c
  \once\override Staff.TimeSignature.stencil =
  \add-interch-ts-dsh #'(3 4)
  \time 9/8
  c4. c8 c c c4 c8
  \omit Score.TimeSignature
  \time 3/4
  c8 c c c c4
}