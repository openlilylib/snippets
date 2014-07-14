\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Predicates for LilyPond version numbers"
  snippet-short-description = \markup {
    Compare the currently running LilyPond version
    against a given version.
  }
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
  snippet-category = "helpers"
  tags = "control-flow,conditionals,compatibility,lilypond-version"
  status = "ready"

  snippet-todo = "Typechecking for the ver-list argument"

  test-field = "rrr"
  test-field-2 = \markup {
    This is a
    multiline
    test field entry
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (calculate-version ver-list)
   ;; take a LilyPond version number as a three element list
   ;; and calculate an integer representation
   (+ (* 1000000 (first ver-list))
      (* 1000 (second ver-list))
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
