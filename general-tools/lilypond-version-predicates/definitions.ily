%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib/openlilylib                                 %
%              -----------                                                    %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2015                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{
  This file provides LilyPond version comparison operators
  which are backported from LilyPond 2.19.57 (scm/lily-library.scm).
  This can be removed if one day support for 2.18 is dropped.

  This implementation is only relevant for snippets defined in the
  openLilyLib/snippets repository meant to work standalone, i.e.
  without including oll-core. If oll-core is loaded, the functions
  will be present anyway.
%}


\header {
  oll-title = "Predicates for LilyPond version numbers"
  oll-short-description = \markup {
    Compare the currently running LilyPond version
    against a given version.
  }
  oll-author = "Urs Liska"
  oll-description = \markup \wordwrap {
    This snippet provides a set of predicates (or comparison
    operators) for LilyPond version numbers. This is useful
    for implementing switches that execute code depending on
    the LilyPond version that is currently running, for example
    when you have to accept different versions.
  }
  oll-usage = \markup \wordwrap {
    Compare the predicates against a LilyPond version number (formatted
    as either a three element list or as a version string). The functions
    will return "#t" or "#f" depending on the result of the comparison.
  }
  oll-category = "utilities"
  oll-tags = "internal,control-flow,conditionals,compatibility,lilypond-version"
  oll-status = "ready"
}


#(if (not (defined? 'lexicographic-list-compare?))
     (define (lexicographic-list-compare? op a b)
       "Lexicographically compare two lists @var{a} and @var{b} using
        the operator @var{op}. The types of the list elements have to
        be comparable with @var{op}. If the lists are of different length
        the trailing elements of the longer list are ignored."
       (let* ((ca (car a))
              (iseql (op ca ca)))
         (let loop ((ca ca) (cb (car b)) (a (cdr a)) (b (cdr b)))
           (let ((axb (op ca cb)))
             (if (and (pair? a) (pair? b)
                      (eq? axb iseql (op cb ca)))
                 (loop (car a) (car b) (cdr a) (cdr b))
                 axb))))))

#(if (not (defined? 'ly:version?))
     (define (ly:version? op ver)
       "Using the operator @var{op} compare the currently executed LilyPond
        version with a given version @var{ver} which is passed as a list of
        numbers."
       (lexicographic-list-compare? op (ly:version) ver)))


%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;; Comparison operators for the currently executed LilyPond version.
%;; Can be used to conditionally execute code based on LilyPond version
%;;
%;; All operators expect a LilyPond version as a string or as a three item list.

#(define (show-outdated func-name)
   (ly:input-warning (*location*)
     "

You are (implicitly) calling an outdated version of '~a' from openlilylib/snippets.
For future compatibility please use 'ly:version? op version-list instead" func-name))

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
  (show-outdated "lilypond-greater-than?")
  (> (calculate-version (ly:version))
     (calculate-version ref-version)))

#(define (lilypond-greater-than-or-equal? ref-version)
  "Return #t if the executed LilyPond version
   is greater than or equal to the given reference version"
  (show-outdated "lilypond-greater-than-or-equal?")
  (>= (calculate-version (ly:version))
      (calculate-version ref-version)))

#(define (lilypond-less-than? ref-version)
  "Return #t if the executed LilyPond version
   is less than the given reference version"
  (show-outdated "lilypond-less-than?")
  (< (calculate-version (ly:version))
     (calculate-version ref-version)))

#(define (lilypond-less-than-or-equal? ref-version)
  "Return #t if the executed LilyPond version
   is less than or equal to the given reference version"
  (show-outdated "lilypond-less-than-or-equal?")
  (<= (calculate-version (ly:version))
      (calculate-version ref-version)))

#(define (lilypond-equals? ref-version)
  "Return #t if the executed LilyPond version
   is equal to the given reference version"
  (show-outdated "lilypond-equals?")
  (= (calculate-version (ly:version))
     (calculate-version ref-version)))

#(define (lilypond-version-string ver-list)
   "Return a string representation of a version list.
    Elements of the list can be either strings or integers"
   (string-join
    (map (lambda (elt)
           (if (integer? elt)
               (number->string elt)
               elt))
      ver-list)
    "."))
