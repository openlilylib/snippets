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


; store values by path

(define-module (scheme-lib lalily registry))

(use-modules (lily)(scheme-lib lalily storage))

(define-public (get-registry-val key . def) #f)
(define-public (set-registry-val key val) #f)
(define-public (display-registry) #f)
(let ((vals (tree-create 'registry)))
  (set! get-registry-val (lambda (key . def)
                           (let ((ret (tree-get vals (if (list? key) key (list key)))))
                             (if ret ret (if (> (length def) 0)(car def) #f)))))
  (set! set-registry-val (lambda (key val) (tree-set! vals (if (list? key) key (list key)) val)))
  (set! display-registry (lambda () (tree-display vals
                                      `(vformat .
                                         ,(lambda (v)
                                            (let ((str (if (markup? v)
                                                           (markup->string v)
                                                           (format "~A" v)
                                                           )))
                                              (if (> (string-length str) 79)
                                                  (string-append
                                                   (substring/read-only str 0 76) " ...") str)) )))))
  )

