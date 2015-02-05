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
  Provide common functionality that is automatically available for
  any library after including the main 'openlilylib' file.
;;
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
    operators) for LilyPond version numbers.
    This is useful for implementing switches in functions
    to execute code depending on the LilyPond version
    that is currently running.

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
