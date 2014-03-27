;;;; This file is part of the lalily section in openlilylib
;;;;
;;;; Copyright (C) 2011--2014 Jan-Peter Voigt <jp.voigt@gmx.de>
;;;;
;;;; lalily is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; lalily is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with lalily.  If not, see <http://www.gnu.org/licenses/>.


; in this module are utility functions collected, which are needed, but do not fit under another category

(define-module (scheme-lib lalily utilities))

(use-modules (lily))

; set all values from on association-list in another one
(define-public (assoc-set-all! lst vls)
  "set all values from a-list `vls` in a-list `lst`"
  (begin
   (for-each (lambda (p)
               (set! lst (assoc-set! lst (car p) (cdr p)))) vls)
   lst))


; make a positive integer a string made of upper-case-letters,
; so that it can be used in list-arguments with dot-notation
; TODO it works, but is not really nice
(define-public (base26 i)
  "produce a string A, B, ..., Z, AA, AB, ... for numbers
usable to allow 2.17+ list input like in: \\editionMod notes.sop.Voice.A
ATTENTION: there will be no ZZ but YZ -> AAA and YZZ -> AAAA"
  (let ((A (char->integer (if (< i 0) #\a #\A)))
        (i (if (< i 0) (- -1 i) i)))

    (define (baseX x i)
      (let ((q (quotient i x))
            (r (remainder i x)))
        (if (and (> q 0) (< q x))
            (list (- q 1) r)
            (let ((ret '()))
              (if (> q 0) (set! ret (baseX x q)))
              `(,@ret ,r))
            )))

    (list->string
     (map
      (lambda (d) (integer->char (+ A d)))
      (baseX 26 i)))
    ))

; every now and then we need a symbol:
; This method creates one from an arbitrary object and is in many cases equivalent to (string->symbol (object->string v))
(define-public (object->symbol o)
    "create symbol from any object"
   (cond
    ((symbol? o) o)
    ((string? o) (string->symbol o))
    ((list? o) (glue-symbol o))
    (else (string->symbol (object->string o display)))
    ))

; like string-join, but with arbitrary objects in the list
(define-public (glue-list lst glue)
  "create string from list containing arbitrary objects"
  (string-join (map (lambda (s) (format "~A" s)) lst) glue 'infix))
; like (string->symbol (string-join l)) but with arbitrary objects
(define-public (glue-symbol lst . glue)
  "create symbol from list containig arbitrary objects"
  (string->symbol (string-join (map (lambda (s) (format "~A" s)) lst) (if (> (length glue) 0)(car glue) ":") 'infix)))

; custom string representation of a moment
(define-public (moment->string mom)
  "produce a human readable string from a moment"
  (if (ly:moment? mom)
      (let ((num (ly:moment-main-numerator mom))
            (den (ly:moment-main-denominator mom))
            (gnum (ly:moment-grace-numerator mom))
            (gden (ly:moment-grace-denominator mom)))
        (format "(~A/~A~A)" num den
          (cond
           ((> gnum 0)(format "+~A/~A" gnum gden))
           ((< gnum 0)(format "~A/~A" gnum gden))
           (else "")
           ))
        )
      "(?:?)"
      ))
