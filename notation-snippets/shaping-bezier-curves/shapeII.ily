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

         (define (is-null-spec? x)
           (eq? '() x))

         (define (is-simple-offset-spec? x)
           (number-pair? x))
         ;; simple offset, the same as \shape was using till now:
         (define (simple-offset x y)
           (cons (+ (car x) (car y))
             (+ (cdr x) (cdr y))))

         (define (is-smart-offset-spec? x)
           (and (list? x)
                (every number? x)))
         ;; flip offset values for right points and downward slurs:
         (define (smart-offset x y i)
           (cons (+ (car x)(* i (first y)))
             (+ (cdr x) (* slur-dir (second y)))))

         (define (handle-one-ctrpt dflt spec idx)
           (cond ((is-null-spec? spec) dflt)
             ((is-simple-offset-spec? spec)(simple-offset dflt spec))
             ((is-smart-offset-spec? spec)(smart-offset dflt spec idx))
             (else (display "Error: unknown specification type"))))

         ;; make \shape #'((foo)) equivalent to \shape #'((foo foo foo foo))
         ;; and \shape #'((foo bar)) to \shape #'((foo bar bar foo)):
         (set! offsets
               (cond
                ((= 1 (length offsets))
                 (make-list 4 (car offsets)))
                ((= 2 (length offsets))
                 (list (first offsets)
                   (second offsets)
                   (second offsets)
                   (first offsets)))
                (else offsets)))

         (if (null? offsets)
             default-cpts
             (map (lambda (x y z)
                    (handle-one-ctrpt x y z))
               ;; the last list is for indicating whether we're modifying
               ;; a control-point on the left or on the right:
               default-cpts offsets '(1 1 -1 -1))))
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
       ;; end of 'handle-one-sibling' routine. ;;
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


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
