;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                             ;
;; This file is part of openLilyLib,                                           ;
;;                      ===========                                            ;
;; the community library project for GNU LilyPond                              ;
;; (https://github.com/openlilylib/openlilylib                                 ;
;;              -----------                                                    ;
;;                                                                             ;
;; openLilyLib is free software: you can redistribute it and/or modify         ;
;; it under the terms of the GNU General Public License as published by        ;
;; the Free Software Foundation, either version 3 of the License, or           ;
;; (at your option) any later version.                                         ;
;;                                                                             ;
;; openLilyLib is distributed in the hope that it will be useful,              ;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of              ;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               ;
;; GNU General Public License for more details.                                ;
;;                                                                             ;
;; You should have received a copy of the GNU General Public License           ;
;; along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         ;
;;                                                                             ;
;; openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  ;
;; and others.                                                                 ;
;;       Copyright Urs Liska, 2015                                             ;
;;                                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Provide tools to OS-independently work with file paths.
;; Compiled and refactored by Urs Liska, based heavily on work by Jan-Peter Voigt
;;

(define-module (_internal utilities os-path))

(use-modules
 (lily)
 (ice-9 regex))

;; #t when running a Windows OS
(define-public is-windows
  (let ((os (getenv "OS")))
    (if (and (string? os)
             (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os))
        #t #f)))

;; forward or backward slash depending on OS
(define-public os-path-separator
  (if is-windows
      #\\
      #\/ ))

(define-public (split-path path)
  "Returns a list with path elements.
    Takes either a path string or a list.
    If 'path' is a string it is split
    respecting the OS dependent path separator,
    if it is a list then simply the list is returned."
  (if (string? path)
      (string-split path os-path-separator)
      path))

(define-public (join-unix-path path-list)
  "Returns a Unix formatted path string from a list."
  (string-join path-list "/"))

(define-public (join-dot-path path)
  "Returns a string in dot-notation (to be displayed).
   Takes a list with string elements or an
   OS independent path string."
  (let ((path-list (split-path path)))
    (string-join path-list ".")))

(define-public (get-cwd-list)
  "Return the current working directory as a list of strings."
  (split-path (getcwd)))

(define-public (absolute-path? path)
  "Test if the given path is absolute.
    Process either a string or a string list."
  (let ((path-list (split-path path)))
    (if (and (> (length path-list) 0)
             ;; consider the path absolute if either the regex for windows volumes is matched
             ;; or the first list element is empty (indicating a "/" unix root)
             (or (regexp-exec (make-regexp "^[a-z]:$" regexp/icase) (car path-list))
                 (= 0 (string-length (car path-list)))))
        #t #f)))


(define-public (normalize-path path)
  "Return a normalized path by removing '.' and '..' elements.
    If 'path' is a string a normalized string is returned,
    if it is a list a list is returned.
    The input string is OS independent (takes os-dependent path separator)
    but the resulting string is Unix-like (because this is nearly always what we need."
  (let* ((path-list (split-path path))
         (normalized
          (let ((ret '()))
            (for-each
             (lambda (e)
               (set! ret (cond ((equal? e "..")(if (> (length ret) 1) (cdr ret) (cdr (reverse (listcwd)))))
                           ((equal? e ".") (if (= (length ret) 0) (reverse (listcwd)) ret))
                           (else `(,e ,@ret))))) path-list)
            (reverse ret))))
    (if (string? path)
        (string-join normalized "/" 'infix)
        normalized)))

(define-public (absolute-path path)
  "Return absolute path of given 'path'.
    Path can be given as string or string list.
    If 'path' is an absolute path it is simply normalized,
    if it is a relative path it is interpreted as relative 
    to the current working directory.
    Input is OS independent, output is Unix style."
  (let* ((is-string (string? path))
         (path-list (split-path path))
         (abs-path
          (if (absolute-path? path-list)
              path-list
              (append
               (get-cwd-list)
               (normalize-path path-list)))))
    (if is-string
        (string-join abs-path "/" 'infix)
        abs-path)))

(define-public (normalize-location location)
  "Returns a normalized path to the given location object"
  (car (ly:input-file-line-char-column location)))

(define-public (location-extract-path location)
  "Returns the normalized path from a LilyPond location
    or './' if 'location' is in the same directory."
  (let* ((loc (normalize-location location))
         (dirmatch (string-match "(.*/).*" loc))
         (dirname (if (regexp-match? dirmatch) (match:substring dirmatch 1) "./")))
    (normalize-path dirname)))

;; Return the normalized absolute path and file name of the
;; file where this function is called from (not the one that
;; is compiled by LilyPond).
(define-public thisFile
  (define-scheme-function (parser location)()
    (normalize-location location)))

(define-public (this-file-compiled parser location)
  "Return #t if the file where this function is called
    is the one that is currently compiled by LilyPond."
  (let ((outname (ly:parser-output-name parser))
        (locname (normalize-location location)))
    (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))))

;; LilyPond format wrapper for this-file-compiled
(define-public thisFileCompiled
  (define-scheme-function (parser location)()
    (this-file-compiled parser location)))
