\version "2.16.0" % This should also work with older versions 

\include "./definitions.ily"


\header {
  title = "LilyPond Version Predicates"
  subtitle = \markup {Score compiled with LilyPond #(lilypond-version)}
}

\paper {
  ragged-right = ##f
}

versionCommentA = 
#(define-music-function (parser location ver)
   (list?)
   (cond ((lilypond-greater-than-or-equal? ver)
          #{ s^\markup {#(lilypond-version) is higher or equals that.} #})
         ((lilypond-less-than-or-equal? ver)
          #{ s^ \markup {#(lilypond-version) is less or equals that.} #})))

versionCommentB = 
#(define-music-function (parser location ver)
   (list?)
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

{
  \tempo "Comparing with: 2.16.0"
  s1
  \versionCommentA #'(2 16 0)
  \versionCommentB #'(2 16 0)
}

\markup { \vspace #2 }

{
  \tempo "Comparing with: 2.16.2"
  s1
  \versionCommentA #'(2 16 2)
  \versionCommentB #'(2 16 2)
}

\markup { \vspace #2 }

{
  \tempo "Comparing with: 2.17.5"
  s1
  \versionCommentA #'(2 17 5)
  \versionCommentB #'(2 17 5)
}

\markup { \vspace #2 }

{
  \tempo "Comparing with: 2.17.16"
  s1
  \versionCommentA #'(2 17 16)
  \versionCommentB #'(2 17 16)
}
