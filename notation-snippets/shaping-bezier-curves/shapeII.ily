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


#(define (single-point-spec? x)
   (or (number-pair? x)
       (and (not (null? x))
            (number? (car x)))))

shapeII =
#(define-music-function (parser location all-offsets item)
   (list? symbol-list-or-music?)
   (_i "TODO: write description when finished")
   (define (shape-curve grob)
     (let* ((orig (ly:grob-original grob))
            (siblings (if (ly:spanner? grob)
                          (ly:spanner-broken-into orig) '()))
            (total-found (length siblings))
            (get-cpts (assoc-get 'control-points
                        (reverse (ly:grob-basic-properties grob))))
            (default-cpts (get-cpts grob))
            (slur-dir (ly:grob-property grob 'direction)))

       (define (handle-one-sibling offsets)
         ;; 'offsets' is a set of instructions for one sibling.

         ;; Allow (0 0) as a shorthand for (0 . 0).
         (set! offsets (list->pair-list offsets))

         ;; convert () to (0 . 0).
         (set! offsets
               (map (lambda (elem)
                      (if (pair? elem)
                          elem
                          (cons 0 0)))
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
               (cond
                ((= 1 (length offsets))
                 (make-list 4 (car offsets)))
                ((= 2 (length offsets))
                 (list (first offsets)
                   (second offsets)
                   (cons (- (car (second offsets)))
                     (cdr (second offsets)))
                   (cons (- (car (first offsets)))
                     (cdr (first offsets)))))
                (else offsets)))

         ;; For downward slurs, flip the offsets vertically
         ;; so that the same override could be applied to similar
         ;; upward and downward slurs.
         (if (eq? slur-dir DOWN)
             (set! offsets
                   (map (lambda (elem)
                          (cons (car elem)
                            (* -1 (cdr elem))))
                     offsets)))

         (if (null? offsets)
             default-cpts
             (map (lambda (x y)
                    (coord-translate x y))
               default-cpts offsets)))
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; end of 'handle-one-sibling' routine.

       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (handle-one-sibling (car offs))
                 (helper (cdr sibs) (cdr offs)))
             default-cpts))

       ;; normalize all-offsets, so that it always is a
       ;; list-of-lists-of-single-point-specs.
       (if (or (null? all-offsets)
               (any single-point-spec? all-offsets))
           (set! all-offsets (list all-offsets)))

       ;; if there are more siblings than specifications,
       ;; reuse last specification for remaining siblings.
       (if (> (- total-found (length all-offsets)) 0)
           (append! all-offsets
             (list (last all-offsets))))

       (if (>= total-found 2)
           (helper siblings all-offsets)
           (handle-one-sibling (car all-offsets)))))

   #{ \tweak control-points #shape-curve #item #})
