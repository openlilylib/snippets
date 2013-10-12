shape =
#(define-music-function (parser location offsets item)
   (list? symbol-list-or-music?)
   (_i "Offset control-points of @var{item} by @var{offsets}.  The
argument is a list of number pairs or list of such lists.  Each
element of a pair represents an offset to one of the coordinates of a
control-point.  If @var{item} is a string, the result is
@code{\\once\\override} for the specified grob type.  If @var{item} is
a music expression, the result is the same music expression with an
appropriate tweak applied.")
   (define (shape-curve grob)
     (let* ((orig (ly:grob-original grob))
            (siblings (if (ly:spanner? grob)
                          (ly:spanner-broken-into orig) '()))
            (total-found (length siblings))
            (function (assoc-get 'control-points
                        (reverse (ly:grob-basic-properties grob))))
            (coords (function grob))
            (dir (ly:grob-property grob 'direction)))

       (define (offset-control-points offsets)
         (if (null? offsets)
             coords
             (map
              (lambda (x y) (coord-translate x y))
              coords offsets)))

       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (offset-control-points (car offs))
                 (helper (cdr sibs) (cdr offs)))
             coords))

       ;; we work with lists of lists
       (if (or (null? offsets)
               (not (list? (car offsets))))
           (set! offsets (list offsets)))

       ;; if only one pair of offsets is supplied,
       ;; use it for all control-points.  I.e.,
       ;; \shape #'((0 . 2)) should be equivalent to
       ;; \shape #'((0 . 2)(0 . 2)(0 . 2)(0 . 2))
       (set! offsets
             (map (lambda (onesib)
                    (if (= 1 (length onesib))
                        (list (car onesib)(car onesib)(car onesib)(car onesib))
                        onesib))
               offsets))

       ;; if two pairs of offsets are supplied,
       ;; use them X-symmetrically for the other two.  I.e.,
       ;; \shape #'((-1 . 1)(2 . 5)) should be equivalent to
       ;; \shape #'((-1 . 1)(2 . 5)(-2 . 5)(1 . 1))
       (set! offsets
             (map (lambda (onesib)
                    (if (= 2 (length onesib))
                        (list (first onesib)
                          (second onesib)
                          (cons (* -1 (car (second onesib)))
                            (cdr (second onesib)))
                          (cons (* -1 (car (first onesib)))
                            (cdr (first onesib))))
                        onesib))
               offsets))

       ;; For downward slurs, flip the offsets vertically
       ;; so that the same override could be applied to similar
       ;; upward and downward slurs.
       (if (eq? dir DOWN)
           (set! offsets
                 (map
                  (lambda (onesib)
                    (map
                     (lambda (oneoff)
                       (cons (car oneoff)
                         (* -1 (cdr oneoff))))
                     onesib))
                  offsets)))

       (if (>= total-found 2)
           (helper siblings offsets)
           (offset-control-points (car offsets)))))
   #{ \tweak control-points #shape-curve #item #})
