\version "2.18.0"

\include "openlilylib"

\useLibrary ScholarLY

\useModule scholarly.annotate

#(display "loaded\n")

%\registerOption documentation.include-file "scholarly/annotate"
%\loadModule "_internal/doc-include/usage-example.ily"

\markup \vspace #1

\setOption scholarly.annotate.export-targets #'("plaintext" "latex")

music = \relative c'{
  c4 d e
    \criticalRemark \with {
      message = "Go \"to \textit{school} \vand \noindent{sit back}!"
      context = "Some staff"
    }
    NoteHead
    f ( |
  g a ) b c
  \musicalIssue \with {
    message = "Another test"
  }
  Staff.KeySignature
  \key a \major
  a d
    <<
      {
        \voiceOne
        \criticalRemark \with {
          message = "go to @\emph{school}@!"
        }
        Accidental
        cis? d
      }
      \new Voice {
        \voiceTwo
        ais b
      }
    >>
    \oneVoice
    cis d
}

\score {
  \new Staff = "My Staff" \music
}