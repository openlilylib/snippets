\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Predicates for LilyPond Version numbers"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    This snippet provides a set of predicates (or comparison
    operators) for LilyPond version numbers.
    This is useful for implementing switches in functions
    to execute code depending on the LilyPond version
    that is currently running.

    The functions take a LilyPond version number (formatted
    as a three element list) as argument and
    compare that to the version number of the running LilyPond.
  }
  % add comma-separated tags to make searching more effective:
  tags = "Program flow, LilyPond versions"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (calculate-version ver-list)
   ;; take a LilyPond version number as a three element list
   ;; and calculate a integer representation
   (+ (* 10000 (first ver-list)) 
      (* 100 (second ver-list)) 
      (third ver-list)))

#(define (lilypond-greater-than? ref-version)
   (> (calculate-version (ly:version))
      (calculate-version ref-version)))

#(define (lilypond-greater-than-or-equal? ref-version)
   (>= (calculate-version (ly:version))
       (calculate-version ref-version)))

#(define (lilypond-less-than? ref-version)
   (< (calculate-version (ly:version))
      (calculate-version ref-version)))

#(define (lilypond-less-than-or-equal? ref-version)
   (<= (calculate-version (ly:version))
       (calculate-version ref-version)))

#(define (lilypond-equals? ref-version)
   (= (calculate-version (ly:version))
      (calculate-version ref-version)))



%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%

%{
versionComment = 
#(define-music-function (parser location ver)
   (list?)
   (if (lilypond-greater-than? ver)
       #{ c'^\markup "Higher" #}
       #{ c'_\markup "Lower/Equal" #}))

{
  c'1^\markup "2.16.2"
  \versionComment #'(2 16 2)
}

{
  c'1^\markup "2.17.5"
  \versionComment #'(2 17 5)
}

%}
