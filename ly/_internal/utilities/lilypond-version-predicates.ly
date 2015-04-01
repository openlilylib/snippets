\version "2.16.0" % This should also work with older versions

\include "openlilylib"
\registerOption documentation.include-file "_internal/utilities/lilypond-version-predicates.ily"

\loadModule "_internal/doc-include/usage-example.ily"

\paper {
  ragged-right = ##f
  indent = 0\cm
}

versionCommentA =
#(define-music-function (parser location ver)
   (scheme?)
   (cond ((lilypond-greater-than-or-equal? ver)
          #{ s^\markup {#(lilypond-version) is higher or equals that.} #})
         ((lilypond-less-than-or-equal? ver)
          #{ s^ \markup {#(lilypond-version) is less or equals that.} #})))

versionCommentB =
#(define-music-function (parser location ver)
   (scheme?)
   (cond ((lilypond-greater-than? ver)
          #{ s^\markup {#(lilypond-version) is higher} #})
         ((lilypond-equals? ver)
          #{ s^\markup {#(lilypond-version) is equal} #})
         ((lilypond-less-than? ver)
          #{ s^\markup {#(lilypond-version) is less} #})))

\markup { \vspace #3 }

\markup \justify {
  Define a few music functions that produce output
  depending on the result of LilyPond version comparisons.
  Compile this file with different versions of LilyPond
  and see how the output markups change.
}

\markup { \vspace #3 }

\markup \bold { Compiled with LilyPond #(lilypond-version) }

\markup { \vspace #3 }

{
  \tempo "Comparing with: 2.18.2"
  s1
  \versionCommentA #'(2 18 2)
  \versionCommentB #'(2 18 2)
}

\markup { \vspace #2 }

{
  \tempo "Comparing with: 2.19.5"
  s1
  \versionCommentA "2.19.5"
  \versionCommentB "2.19.5"
}

\markup { \vspace #2 }

{
  \tempo "Comparing with: 2.19.16"
  s1
  \versionCommentA #'(2 19 16)
  \versionCommentB #'(2 19 16)
}

{
  \tempo "Comparing with: 2.20.0"
  s1
  \versionCommentA "2.20.0"
  \versionCommentB "2.20.0"
}

\markup { \vspace #2 }

