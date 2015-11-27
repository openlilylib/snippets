\version "2.19.32"

% Take the ratio of a partial and return the corresponding cent value
% relative to a fundamental
#(define (ratio->cent ratio)
   (* 12 (/ (log (/ (car ratio) (cdr ratio))) (log 2))))

% Take a fraction and return a list with 
% - the pitch in semitones
% - the cent deviation above or below (rounded)
#(define (ratio->step-deviation ratio)
   (let*
    ;; calculate cent value over the fundamental
    ((step-cent (ratio->cent ratio))
     ;; split that in the step and the cent part
     (step (inexact->exact (round step-cent)))
     (cent (inexact->exact (round (* 100 (- step-cent (floor step-cent))))))
     ;; if cent > 50 flip it around to be negative
     (cent-deviation
      (if (> cent 50)
          (- cent 100)
          cent)))
    (cons step cent-deviation)))

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


