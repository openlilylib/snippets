\version "2.19.32"

% Take the ratio of a partial and return the corresponding cent value
% relative to a fundamental
#(define (ratio->cent ratio)
   (* 1200 (/ (log (/ (car ratio) (cdr ratio))) (log 2))))

% Return a pitch object
% corresponding to the given semitone
% (relative to c')
#(define (semitones->pitch semitone)
   (let* ((index (modulo semitone 12))
          (octave (floor (/ semitone 12)))
          (pitch-pair 
           (list-ref 
            '((0 0)   ; c
               (0 1/2) ; cis
               (1 0)   ; d
               (1 1/2) ; dis
               (2 0)   ; e
               (3 0)   ; f
               (3 1/2) ; fis %  \jiPitch 2 1
  
               (4 0)   ; g
               (4 1/2) ; gis
               (5 0)   ; a
               (6 -1/2) ; ais
               (6 0))   ; b      
            index)))
     (ly:make-pitch octave (car pitch-pair) (cadr pitch-pair))))

% Produce a color based on the cent detune.
% Positive detunes color increasingly red
% while negative colors produce shades of blue
#(define (cent->color cent)
   (let
    ((r (if (> cent 0)
            (sqrt (/ cent 50.0))
            0.0))
     (b (if (< cent 0)
            (sqrt (* -1 (/ cent 50.0)))
            0.0)))
    (list r 0.0 b)))

