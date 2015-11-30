\version "2.19.32"

% Take the ratio of a partial and return the corresponding cent value
% relative to a fundamental
#(define (ratio->cent ratio)
   (* 1200 (/ (log (/ (car ratio) (cdr ratio))) (log 2))))

% Extract the semitone part of a cent value
#(define (cent->semitone cent)
   (inexact->exact (round (/ cent 100.0))))

% Calculate the cent deviation from the tempered pitch
% Expected argument is either
% - the total cent value in relation to the fundamental or
% - a pair of total-cent and an (already determined) semitone step
% The result will be a number -50 < N < 50
#(define (cent->deviation pitch)
   (if (number? pitch)
       (inexact->exact (round (- pitch (* 100 (cent->semitone pitch)))))
       (inexact->exact (round (- (car pitch) (* 100 (cdr pitch)))))))

#(define (ratio->note fundamental ratio)
   (let*
    ((note '())
     (cent (ratio->cent ratio))
     (semitone (cent->semitone cent))
     (deviation (cent->deviation (cons cent semitone))))
    (set! note
          (assoc-set! note "fundamental" fundamental))
    (set! note 
          (assoc-set! note "cent" cent))
    (set! note
          (assoc-set! note "semitone" semitone))
    (set! note
          (assoc-set! note "pitch" 
            (ly:pitch-transpose (semitones->pitch semitone) fundamental)))
    (set! note
          (assoc-set! note "deviation" deviation))
    note))
    

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
               (6 -1/2) ; bes
               (6 0))   ; b
            index)))
     (ly:make-pitch octave (car pitch-pair) (cadr pitch-pair))))

% Produce a color based on the cent detune.
% Positive detunes color increasingly red
% while negative colors produce shades of blue
#(define (deviation->color deviation)
   (let
    ((r (if (> deviation 0)
            (sqrt (/ deviation 50.0))
            0.0))
     (b (if (< deviation 0)
            (sqrt (* -1 (/ deviation 50.0)))
            0.0)))
    (list r 0.0 b)))

