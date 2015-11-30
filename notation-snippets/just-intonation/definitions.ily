\version "2.19.32"

% Proof-of-concept for a "just intonation" input syntax

\include "openlilylib"

\registerOption contemporary #'()
\registerOption contemporary.ji #'()
\registerOption contemporary.ji.duration #(ly:make-duration 2)

#(ly:set-option 'relative-includes #t)
\include "conversions.ily"

% Maintain a current duration to be used when no duration is given
% This is extremely hacky and will only work in monophonic context
#(define ji-duration (ly:make-duration 2))

% Iterate over a set of grobs and color them with the given color
#(define (color-music color)
   (map 
    (lambda (g)
       #{ \once \override #g . color = #color #})
    (list 
     'Accidental
     'NoteHead
     'Stem
     'Flag
     'TextScript)))


% Produce a note in Just Intonation
%
% The function expects three arguments:
% - fundamental note, over which the ratio is applied
% - (optional) duration. 
%   If this isn't present the previous duration is used.
%   This results in a duration behaviour that is similar to LilyPond's own
%   but it is limited to the \ji context.
% - ratio
%
% The return value is a note with the specified duration and an 
% attached markup with a string representation of the deviation in cent.
ji =
#(define-music-function (fundamental dur ratio)
   (ly:pitch? (ly:duration?) fraction?)
   (let*
    ((note (ratio->note fundamental ratio))
     (cent (assoc-ref note "cent"))
     (pitch (assoc-ref note "pitch"))
     (deviation (assoc-ref note "deviation"))
     (col (deviation->color deviation)))
    ;; Update current duration if given as argument
    (if dur (setOption '(contemporary ji duration) dur))
    ;; produce a note from the given data
    #{ #@(color-music col) 
       $pitch 
       $(getOption '(contemporary ji duration))
       ^#(format "(~@d)" deviation) #}))
