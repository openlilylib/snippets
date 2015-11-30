\version "2.19.32"

% Proof-of-concept for a "just intonation" input syntax

#(ly:set-option 'relative-includes #t)
\include "conversions.ily"

% Maintain a current duration to be used when no duration is given
% This is extremely hacky and will only work in monophonic context
#(define ji-duration (ly:make-duration 2))

% Produce the code for coloring one grob in a (make-music) expression
#(define (color-element grob color)
   (make-music
    'ContextSpeccedMusic
    'context-type
    'Bottom
    'element
    (make-music
     'OverrideProperty
     'once
     #t
     'pop-first
     #t
     'grob-value
     color
     'grob-property-path
     (list (quote color))
     'symbol
     grob)))

% Iterate over a set of grobs and color them with the given color
% Note that this returns a list that can't simply replace the consecutive
% calls to color-element. Instead the actual (make-music) has to be appended
% to the result of color-music.
#(define (color-music color)
   (map 
    (lambda (g)
      (color-element g color))
    (list 
     'Accidental
     'NoteHead
     'Stem
     'Flag
     'TextScript)))


% Produce a note displaying Just Intonation
% Provide a ratio, which is currently taken to be over c'
% The duration will be taken from the currently active "ji-duration"
% which can be changed with the optional argument.
% The function will return a note with the tempered pitch that
% matches the actual pitch most closely, and a markup with
% the rounded cent deviation.
ji =
#(define-music-function (fundamental dur ratio)
   (ly:pitch? (ly:duration?) fraction?)
   (let*
    ((note (ratio->note ratio))
     (cent (assoc-ref note "cent"))
     (pitch (assoc-ref note "pitch"))
     (deviation (assoc-ref note "deviation"))
     (pitch-transposed (ly:pitch-transpose pitch fundamental))
     (col (deviation->color deviation)))
    ;; Update current duration if given as argument
    (set! ji-duration (or dur ji-duration))
    ;; produce a note from the given data
    #{ #@(color-music col) 
       $pitch-transposed 
       $ji-duration 
       ^#(format "(~@d)" deviation) #}))
