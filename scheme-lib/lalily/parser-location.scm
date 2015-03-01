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


; helper-functions

(define-module (scheme-lib lalily parser-location))

(use-modules
 (lily)
 (ice-9 regex)
 (srfi srfi-1)
 )

;
; All of the following is deprecated
; as the functionality has been moved to
; ly/_internal/utilities/os-path.ily.
;
; There it is much more consistently named and handled,
; and a number of code duplicates have been harmonized.
; The complete file should officially be deprecated.
;
; This deprecation must be used more carefully (i.e. by only
; printing a warning) because code dependencies may be much
; more common and complex than with simply moving a file into
; the new structure (which is because the move goes along with
; significant rewrite).
;


(define-public (lalily-test-location? parser location)
  (let ((outname (ly:parser-output-name parser))
        (locname (car (ly:input-file-line-char-column location))))
    (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))
    ))

(define-public (listcwd) '())
(define-public (absolutePath? path) #f)
(let* ((os (getenv "OS"))
       (isWindows (if (and (string? os) (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os)) #t #f))
       (wrx (if isWindows (make-regexp "^[a-z]:$" regexp/icase) #f)))
  (set! listcwd (lambda () (string-split (getcwd)(if isWindows #\\ #\/))))
  (set! absolutePath? (lambda (path) (if isWindows
                                         (if (and (> (length path) 0) (regexp-exec wrx (car path))) #t #f)
                                         (and (> (length path) 0) (= 0 (string-length (car path))))
                                         )))
  )

(define-public (normalize-path-list path)
  "create list, removing \"..\" elements
example: (normalize-path '(\"a\" \"b\" \"..\" \"c\" \".\" \"d\")) ==> '(\"a\" \"c\" \"d\")"
  (let ((ret '()))
    (for-each (lambda (e)
                (set! ret (cond ((equal? e "..")(if (> (length ret) 1) (cdr ret) (cdr (reverse (listcwd)))))
                            ((equal? e ".") (if (= (length ret) 0) (reverse (listcwd)) ret))
                            (else `(,e ,@ret))))) path)
    (reverse ret)))
(define-public (normalize-path-string s)
  "create normalized path string: a/b/../c/d ==> a/c/d"
  (string-join (normalize-path-list (string-split s #\/)) "/" 'infix))

(define-public (location-extract-path location)
  (let* ((loc (car (ly:input-file-line-char-column location)))
         (dirmatch (string-match "(.*/).*" loc))
         (dirname (if (regexp-match? dirmatch) (match:substring dirmatch 1) "./")))
    (normalize-path-string dirname)
    ))


