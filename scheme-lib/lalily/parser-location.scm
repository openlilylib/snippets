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


(define-module (scheme-lib lalily parser-location))

(use-modules (lily)(ice-9 regex)(srfi srfi-1))

(define-public (lalily-test-location? parser location)
  (let ((outname (ly:parser-output-name parser))
        (locname (car (ly:input-file-line-char-column location))))
    (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))
    ))
