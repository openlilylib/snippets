%{
;; This file is part of the openLilyLib library infrastructure
;; ... TOBEDONE ...
;;
;;
;; Provide common functionality that is automatically available for
;; any library after including the main 'openlilylib' file.
;;
%}



%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;; Comparison operators for the currently executed LilyPond version.
%;; Can be used to conditionally execute code based on LilyPond version
%;;
%;; All operators expect a LilyPond version as a string or as a three item list.

#(define (calculate-version ref-version)
  "Return an integer representation of the LilyPond version,
   can be compared with the operators."
  (let ((ver-list
         (if (list? ref-version)
             ref-version
             (let ((str-list (string-split ref-version #\.)))
               (map
                (lambda (s)
                  (string->number s))
                str-list)))))
    (+ (* 1000000 (first ver-list))
      (* 1000 (second ver-list))
      (third ver-list))))

#(define (lilypond-greater-than? ref-version)
  "Return #t if the executed LilyPond version
   is greater than the given reference version"
  (> (calculate-version (ly:version))
     (calculate-version ref-version)))

#(define (lilypond-greater-than-or-equal? ref-version)
  "Return #t if the executed LilyPond version
   is greater than or equal to the given reference version"
  (>= (calculate-version (ly:version))
      (calculate-version ref-version)))

#(define (lilypond-less-than? ref-version)
  "Return #t if the executed LilyPond version
   is less than the given reference version"
  (< (calculate-version (ly:version))
     (calculate-version ref-version)))

#(define (lilypond-less-than-or-equal? ref-version)
  "Return #t if the executed LilyPond version
   is less than or equal to the given reference version"
  (<= (calculate-version (ly:version))
      (calculate-version ref-version)))

#(define (lilypond-equals? ref-version)
  "Return #t if the executed LilyPond version
   is equal to the given reference version"
  (= (calculate-version (ly:version))
     (calculate-version ref-version)))
