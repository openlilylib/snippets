\version "2.17.15"

\layout {
  ragged-right = ##t
  indent = #0
}


#(define-public (number-pair-list? x)
   (and (pair? x)
        (every number-pair? x)))

foo-slur =
#(define-music-function (parser location lst) (number-pair-list?)
   #{
     \override Slur.control-points =
     #(lambda (grob)
        (let* ((get-cpts (assoc-get 'control-points
                      (reverse (ly:grob-basic-properties grob))))
               (left-bound (ly:spanner-bound grob LEFT))
               (left-yoffset (ly:grob-property left-bound 'Y-offset))
               (cps (get-cpts grob))
               (default-x1 (car (first cps)))
               (default-y1 (cdr (first cps)))
               (default-x4 (car (last cps)))
               (default-y4 (cdr (last cps)))
               (slur-length (- (car (last cps)) default-x1))
               (x1 (+ default-x1 (car (first lst))))
               (y1 (+ default-y1 (cdr (first lst))))
               (x2 (+ default-x1 (* slur-length (car (second lst)))))
               (y2 (+ default-y1 (cdr (second lst))))
               (x3 (+ default-x1 (* slur-length (car (third lst)))))
               (y3 (+ default-y4 (cdr (third lst))))
               (x4 (+ (car (last cps)) (car (last lst))))
               (y4 (+ default-y4 (cdr (last lst)))))

          (list (cons x1 y1)
            (cons x2 y2)
            (cons x3 y3)
            (cons x4 y4))))
   #})

{
  c'2 ( d') | c'4 ( c' d' d' )
  \break
  \once \foo-slur #'((0 . 0) (0.5 . -1.5) (0.9 . -1.5) (0 . 0))
  c'2 ( d') | c'4 ( c' d' d' )
  \break
  a2( d'' a'' d''')
}