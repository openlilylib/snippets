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
