\version "2.17.26"

\header {
  snippet-title = "Improved \shape"
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

shapeII =
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
                        (cons (- (car (second x)))
                          (cdr (second x)))
                        (cons (- (car (first x)))
                          (cdr (first x)))))
                     (else x)))
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
