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
  This files contains utility routines to handle rhythmic-location properties.
  It is written for \annotate but the functions should be generally usable
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions for the annotation engraver
% (provided by David Nalesnik)


#(define (ly:moment<=? mom1 mom2)
   "Compare two moments to determine precedence"
   (or (ly:moment<? mom1 mom2)
       (equal? mom1 mom2)))

#(define (moment-floor num den)
   "Return the number of times den fits completely into num."
   (let loop ((result 1) (num num))
     (if (ly:moment<=? ZERO-MOMENT (ly:moment-sub num den))
         (loop (1+ result) (ly:moment-sub num den))
         result)))

% TODO: What's this?
#(define (compound? sig)
   "Determine if a meter is compound."
   (let ((num (car sig)))
     (cond
      ((= num 3) #f)
      ((= 0 (modulo num 3)) #t)
      (else #f))))

#(define (number-of-beats sig)
   "Return the number of beats in a given time signature"
   (let ((num (car sig))
         (den (cdr sig)))
     (if (compound? sig)
         (begin
          (display "compound")
          (/ num 3))
         num)))

#(define (beat-length measure-length number-of-beats)
   "Return the length of one single 'beat' as a moment"
   (ly:moment-div measure-length (ly:make-moment number-of-beats)))

% From LilyPond 2.19.6 on you can use (ly:item-get-column item)
#(define (get-paper-column grob)
   "Return the paper column of a given grob.
    This property knows about the rhyhmic position in a score"
   (cond
    ((not (ly:grob? grob)) #f)
    ((grob::has-interface grob 'paper-column-interface) grob)
    (else (get-paper-column
           ;; Can't use 'X' for axis because 'X' is also a music variable
           (ly:grob-parent grob 0)))))

#(define (location grob)
   "Return the musical/rhythmical position of a given grob.
    If the position can't be determined return an 'impossible'
    value in measure 0."
   (if (ly:grob? grob)
       (let
        ((loc
          (if (lilypond-greater-than-or-equal? "2.19.16")
              (grob::rhythmic-location grob)
              (let ((col (get-paper-column grob)))
                (if col
                    (ly:grob-property col 'rhythmic-location)
                    '())))))
        (if (null? loc)
            (cons 0 (ly:make-moment 0/4))
            loc))
       (ly:error "Requested rhythmic-location of a grob, but ~a is not a grob," grob)))



% Define beat-string as a procedure so we can later make it configurable
% or at least allow the user to redefine this single procedure
#(define (beat-string props)
   "Return a string representation of the measure position."
   (let*
    ((our-beat (assoc-ref props "our-beat"))
     (beat-fraction (assoc-ref props "beat-fraction"))
     (beat-str (number->string our-beat))
     (beat-str
      (if (= 0 beat-fraction)
          beat-str
          (string-append
           beat-str
           " "
           (number->string beat-fraction)))))
    beat-str))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate the rhythmic properties of an annotation

#(define (grob-location-properties grob props)
   "Populate the alist 'props' with more details about the rhythmic location of 'grob'.
    It is assumed that a property 'meter' has already been set with a time sig pair."
   (let*
    ((loc (location grob))
     (measure-pos (cdr loc))
     (meter (assoc-ref props "meter"))
     (beats-in-meter (car meter))
     (beat-len (ly:make-moment 1 (cdr meter)))
     (our-beat (moment-floor measure-pos beat-len))
     (beat-part (ly:moment-sub
                 measure-pos
                 (ly:moment-mul
                  (ly:make-moment (1- our-beat))
                  beat-len)))
     (beat-fraction (moment->fraction
                     (ly:moment-div beat-part beat-len)))
     (beat-fraction (/ (car beat-fraction) (cdr beat-fraction))))

    (set! props (assoc-set! props "rhythmic-location" loc))
    (set! props (assoc-set! props "measure-no" (car loc)))
    (set! props (assoc-set! props "measure-pos" (cdr loc)))
    (set! props (assoc-set! props "our-beat" our-beat))
    (set! props (assoc-set! props "beat-part" beat-part))
    (set! props (assoc-set! props "beat-fraction" beat-fraction))
    (set! props (assoc-set! props "beat-string" (beat-string props)))

    ;; "return" modified props
    props))
