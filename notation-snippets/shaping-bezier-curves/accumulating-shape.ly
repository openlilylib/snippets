\version "2.17.26"

% From David Nalesnik

#(define (find-value-to-offset prop self alist)
  "Return the first value of the property @var{prop} in the property
alist @var{alist} @em{after} having found @var{self}."
  (let ((segment (member (cons prop self) alist)))
    (if (not segment)
        (assoc-get prop alist)
        (assoc-get prop (cdr segment)))))

#(define (list->pair-list lst)
   (cond ((null? lst) lst)
         ((number-pair? (car lst))
          (cons (car lst) (list->pair-list (cdr lst))))
         ((list? (car lst))
          (cons
            (list->pair-list (car lst))
            (list->pair-list (cdr lst))))
         (else (cons (car lst) (cadr lst)))))

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
            (immutable (ly:grob-basic-properties grob))
            (value (find-value-to-offset 'control-points shape-curve immutable))
            (coords (if (procedure? value)
                        (value grob)
                        value)))

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
       
       (set! offsets (list->pair-list offsets))

       ;; Offsets may be (1) the empty list; (2) A number-pair-list;
       ;; or (3) A list of number-pair-lists.  In order to easily work
       ;; with these possibilities (and not require overly confusing input
       ;; from the user), we normalize (1) and (2), by converting each
       ;; to (3).
       ;; '() ==> '(())
       ;; '((0 . 1) ... ) ==> '( ((0 . 1) ... ) )
       (if (or (null? offsets)
               (not (list? (car offsets))))
           (set! offsets (list offsets)))
       
       ;; if only one pair of offsets is supplied,
       ;; use it for all control-points.  I.e.,
       ;; \shape #'((0 . 2)) should be equivalent to
       ;; \shape #'((0 . 2)(0 . 2)(0 . 2)(0 . 2))
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

       (if (>= total-found 2)
           (helper siblings offsets)
           (offset-control-points (car offsets)))))
   #{ \tweak control-points #shape-curve #item #})

\paper { ragged-right = ##t }
{
  d''1 ( f'')
  \break
  \shape #'((1 . 1)) Slur
  d''1 ( f'')
  \shape #'((1 . 1)) Slur
  d''1-\shape #'((0 . 1)) ( f'')
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  d''1 ( f'')
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  d''1 ( f'')
  
  %% And the piece de resistance...
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  \shape #'((0 . 1)) Slur
  d''1-\shape #'((0 . -6)) ( f'' )
  
  \break
  % offset of an earlier override of 'control-points
  \override Slur #'control-points = #'((0 . 3) (4 . 5) (6 . 5) (10 . 5))
  d'' ( f'' )
  \shape #'((0 . 1)) Slur
  d'' ( f'')
}
