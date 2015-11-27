\version "2.19.32"

% Take the ratio of a partial and return the corresponding cent value
% relative to a fundamental
#(define (ratio->cent ratio)
   (* 12 (/ (log (/ (car ratio) (cdr ratio))) (log 2))))
