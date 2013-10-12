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
               ;(left-bound (ly:spanner-bound grob LEFT))
               ;(left-yoffset (ly:grob-property left-bound 'Y-offset))
               (cps (get-cpts grob))

               (default-x1 (car (first cps)))
               (default-y1 (cdr (first cps)))
               (default-x4 (car (last cps)))
               (default-y4 (cdr (last cps)))

               ;; need to calculate these before we can calculate length
               (x1 (+ default-x1 (car (first lst))))
               (y1 (+ default-y1 (cdr (first lst))))
               (x4 (+ default-x4 (car (last lst))))
               (y4 (+ default-y4 (cdr (last lst))))

               ;; get the distance between first and last control-points
               (x-dif (- x4 x1))
               (y-dif (- y4 y1))
               (slur-length (sqrt (+ (expt x-dif 2) (expt y-dif 2))))

               ;; precomputations for polar coordinates
               (rad2 (* slur-length (cdr (second lst))))
               (rad3 (* slur-length (cdr (third lst))))
               (angle2 (degrees->radians (car (second lst))))
               (angle3 (degrees->radians (- 180 (car (third lst)))))

               ;; measure middle cpts position from NEW positions of outer pts.
               (x2 (+ x1 (* rad2 (cos angle2))))
               (y2 (+ y1 (* rad2 (sin angle2))))
               (x3 (+ x4 (* rad3 (cos angle3))))
               (y3 (+ y4 (* rad3 (sin angle3)))))

          (list (cons x1 y1)
            (cons x2 y2)
            (cons x3 y3)
            (cons x4 y4))))
   #})

\markup \wordwrap {
  Slur shape may be specified using polar coordinates. The syntax is
  \typewriter "((x1off . y1off) (1-2-angle . 1-2-radius) (3-4-angle . 3-4-radius) (x4off . y4off))"
  where \typewriter "(x1off . y1off)" are offsets just like in ordinary "\shape".
  Angles are in degrees, radius is normalized: 1 means the distance between
  outer control-points.
}
{
  c'2 ( d') | c'4 ( c' d' d' )
  \foo-slur #'((0 . 0) (-30 . 0.6) (-90 . 0.3) (0 . 0))
  c'2 ( d')
  c'4 ( c' d' d' )
}
{
  e2( d'' b'' d''')
  \foo-slur #'((0 . -2.5)(88 . 0.5)(20 . 0.2)(0 . 0))
  e2( d'' b'' d''')
}
\markup { S-shaped slurs are trivial to achieve: }
{
  \foo-slur #'((0 . 0) (-50 . 0.5) (50 . 0.5) (-1 . 0))
  a1 ( g)
}