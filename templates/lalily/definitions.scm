;;;; This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
;;;;
;;;; Copyright (C) 2011--2013 Jan-Peter Voigt <jp.voigt@gmx.de>
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

(define-module (templates lalily definitions))

(define-public lalily:paper:global-staff-size '(lalily paper global-staff-size))
(define-public lalily:paper:default '(lalily paper default))

(define-public lalily:test-predicate '(lalily runtime test-predicate))
(define-public lalily:create '(lalily runtime create))

(define-public lalily:store:path-variables '(lalily runtime store path variables))
(define-public lalily:get-music-load-callbacks '(lalily runtime store load callbacks))
(define-public lalily:get-music-store-callbacks '(lalily runtime store store callbacks))
