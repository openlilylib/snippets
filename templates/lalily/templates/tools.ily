\version "2.18.0"

Path =
#(define-scheme-function (parser location p)(list?) p)
PathS =
#(define-scheme-function (parser location s p)((char? #\/) string?)
   (map (lambda (e) (if (> (string-length e) 0) (string->symbol e) '/)) (string-split p s)))
Pair =
#(define-scheme-function (parser location p)(list?)
   (cond
    ((>= (length p) 2)
     (if (> (length p) 2)
     (ly:input-warning location "more than 2 elements: ~A" p))
     (cons (car p) (cadr p)))
    ((> (length p) 0) (cons (car p) #f))
    (else '(#f . #f))
    ))
PairS =
#(define-scheme-function (parser location s p)((char? #\|) string?)
   (ly:music-function-exec Pair parser location
     (string-split p s)))
