\version "2.17.15"

\header {
  snippet-title = "\shape in polar coordinates"
  snippet-author = "Janek Warchoł, David Nalesnik"
  snippet-source = ""
  snippet-description = \markup {
    
  }
  % add comma-separated tags to make searching more effective:
  tags = "slurs, ties, bezier curves, shape"
  % is this snippet ready?  See meta/status-values.md
  status = "working, unfinished"
}

#(define (list->pair-list lst)
   (cond ((null? lst) lst)
     ((number-pair? (car lst))
      (cons (car lst) (list->pair-list (cdr lst))))
     ((list? (car lst))
      (cons
       (list->pair-list (car lst))
       (list->pair-list (cdr lst))))
     (else (cons (car lst) (cadr lst)))))

shape-polar =
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

       (define (polar-control-points offsets)
         (let* ((get-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
                (left-bound (ly:spanner-bound grob LEFT))
                (left-name (get-name left-bound))
                (left-y-extent (ly:grob-property left-bound 'Y-extent))
                (right-bound (ly:spanner-bound grob RIGHT))
                (right-name (get-name right-bound))
                (right-y-extent (ly:grob-property right-bound 'Y-extent))

                ;; in case of cross-staff curves...
                (refp (ly:grob-system grob))
                (left-y-coord (ly:grob-relative-coordinate left-bound refp Y))
                (right-y-coord (ly:grob-relative-coordinate right-bound refp Y))
                (dist-between-staves (- right-y-coord left-y-coord))

                (default-x1 (car (first coords)))
                (default-y1 (cdr (first coords)))
                (default-x4 (car (last coords)))
                (default-y4 (cdr (last coords)))

                ;; need to calculate these before we can calculate length
                (x1 (+ default-x1 (car (first offsets))))
                ;; in case of broken slurs, we don't have a NoteColumn to look at,
                ;; so we simply offset default coords.
                (y1 (if (string=? (symbol->string left-name) "NoteColumn")
                        (if (eq? dir DOWN)
                            ;; 0.6 is the default vert. distance from notehead
                            (- (car left-y-extent)(+ 0.6 (cdr (first offsets))))
                            (+ (cdr left-y-extent)(+ 0.6 (cdr (first offsets)))))
                        (+ default-y1 (cdr (first offsets)))))
                (x4 (+ default-x4 (car (last offsets))))

                ;; UGH!! I have no idea why this is needed, but without this correction
                ;; the example below renders wrongly:
                ;; { d''1-\polar #'(((0 . 0.5) (45 . 0.4) (35 . 0.4) (0 . 1))
                ;; ((0 . 1)(35 . 0.35)(45 . 0.35)(0 . 0))) ( f'' \break a'' g'') }
                ;; the if clause is necessary because otherwise the 'fix' will
                ;; break the cross-staff case.  UGH!!
                (y4-correction
                 (if (ly:grob-property grob 'cross-staff) ; returns boolean
                     0.0
                     (- (car right-y-extent)
                       (car (ly:grob-extent right-bound refp Y)))))

                (y4 (if (string=? (symbol->string right-name) "NoteColumn")
                        (+ dist-between-staves
                          y4-correction
                          (if (eq? dir DOWN)
                              ;; 0.6 is the default vert. distance from notehead
                              (- (car right-y-extent)(+ 0.6 (cdr (last offsets))))
                              (+ (cdr right-y-extent)(+ 0.6 (cdr (last offsets))))))
                        (+ default-y4 (cdr (last offsets)))))

                ;; get the distance between first and last control-points
                (x-dif (- x4 x1))
                (y-dif (- y4 y1))
                (slur-length (sqrt (+ (expt x-dif 2) (expt y-dif 2))))

                ;; precomputations for polar coordinates
                (rad2 (* slur-length (cdr (second offsets))))
                (rad3 (* slur-length (cdr (third offsets))))
                (angle2raw (degrees->radians (car (second offsets))))
                (angle3raw (degrees->radians (- 180 (car (third offsets)))))
                (angle2 (if (eq? dir DOWN)
                            (* -1 angle2raw)
                            angle2raw))
                (angle3 (if (eq? dir DOWN)
                            (* -1 angle3raw)
                            angle3raw))

                ;; measure middle cpts position from NEW positions of outer pts.
                (x2 (+ x1 (* rad2 (cos angle2))))
                (y2 (+ y1 (* rad2 (sin angle2))))
                (x3 (+ x4 (* rad3 (cos angle3))))
                (y3 (+ y4 (* rad3 (sin angle3)))))

           (if (null? offsets)
               coords
               (list (cons x1 y1)
                 (cons x2 y2)
                 (cons x3 y3)
                 (cons x4 y4)))))

       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (polar-control-points (car offs))
                 (helper (cdr sibs) (cdr offs)))
             coords))

       ;; Allow (0 0) as a shorthand for (0 . 0).
       ;; This must be done before next step,
       ;; because we look for pairs there.
       (set! offsets (list->pair-list offsets))

       ;; Offsets may be given in a variety of formats:
       ;; (1) an empty list,
       ;; (2) a number-pair-list (used for unbroken curves), or
       ;; (3) a list of number-pair-lists (for curves crossing a linebreak).
       ;; In order to easily work with these possibilities (and not require
       ;; overly confusing input from the user), we normalize (1) and (2),
       ;; by converting each to (3).
       ;; '() ==> '(())
       ;; '((0 . 1) ... ) ==> '( ((0 . 1) ... ) )
       ;;
       ;; WARNING: since we allow () to be a shorthand for (0 . 0),
       ;; we need to handle tricky constructs like '(()(0 . 1)()())
       ;; (which is a list of offsets for ONE slur, so must be normalized).
       (if (or (null? offsets)
               (any number-pair? offsets))
           (set! offsets (list offsets)))

       ;; convert () to (0 . 0), but only on the appropriate level.
       ;; I.e. '((()(0 . 1)(0 . 1)())) should be converted to
       ;; '(((0 . 0)(0 . 1)(0 . 1)(0 . 0))), but
       ;; '(() ((0 . 1)(0 . 1)(0 . 1)(0 . 1))) shouldn't be converted to
       ;; '((0 . 0) ((0 . 1)(0 . 1)(0 . 1)(0 . 1)))
       (set! offsets
             (map (lambda (onesib)
                    (map (lambda (oneoff)
                           (if (not (pair? oneoff))
                               (cons 0 0)
                               oneoff))
                      onesib))
               offsets))

       ;; if only one pair of offsets is supplied,
       ;; use it for all control-points.  I.e.,
       ;; \shape #'((0 . 2)) should be equivalent to
       ;; \shape #'((0 . 2)(0 . 2)(0 . 2)(0 . 2))
       ;;
       ;; if two pairs of offsets are supplied,
       ;; use them X-symmetrically for the other two.  I.e.,
       ;; \shape #'((-1 . 1)(2 . 5)) should be equivalent to
       ;; \shape #'((-1 . 1)(2 . 5)(-2 . 5)(1 . 1))
       (set! offsets
             (map (lambda (x)
                    (cond
                     ((= 1 (length x))
                      (make-list 4 (car x)))
                     ((= 2 (length x))
                      (list (first x)
                        (second x)
                        ;; different than in original shape, because
                        ;; polar coords behave differently...
                        (cons (car (second x))
                          (cdr (second x)))
                        (cons (- (car (first x)))
                          (cdr (first x)))))
                     (else x)))
               offsets))

       ;; For downward slurs, flip the offsets vertically
       ;; so that the same override could be applied to similar
       ;; upward and downward slurs.

       (if (>= total-found 2)
           (helper siblings offsets)
           (polar-control-points (car offsets)))))
   #{ \tweak control-points #shape-curve #item #})
