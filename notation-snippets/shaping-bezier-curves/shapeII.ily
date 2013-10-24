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
            (or (number? (car x))
                (symbol? (car x))))))

shapeII =
#(define-music-function (parser location all-specs item)
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

       (define (handle-one-sibling specs)
         ;; 'specs' is a set of instructions for one sibling.

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
         (set! specs
               (cond
                ((= 1 (length specs))
                 (make-list 4 (car specs)))
                ((= 2 (length specs))
                 (list (first specs)
                   (second specs)
                   (second specs)
                   (first specs)))
                (else specs)))

         (if (null? specs)
             default-cpts
             (map (lambda (x y z)
                    (handle-one-ctrpt x y z))
               ;; the last list is for indicating whether we're modifying
               ;; a control-point on the left or on the right:
               default-cpts specs '(1 1 -1 -1))))
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
       ;; end of 'handle-one-sibling' routine. ;;
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (handle-one-sibling (car offs))
                 (helper (cdr sibs) (cdr offs)))
             default-cpts))

       ;; normalize all-specs, so that it always is a
       ;; list-of-lists-of-single-point-specs.
       (if (or (null? all-specs)
               (any single-point-spec? all-specs))
           (set! all-specs (list all-specs)))

       ;; if there are more siblings than specifications,
       ;; reuse last specification for remaining siblings.
       (if (> (- total-found (length all-specs)) 0)
           (append! all-specs
             (list (last all-specs))))

       (if (>= total-found 2)
           (helper siblings all-specs)
           (handle-one-sibling (car all-specs)))))

   #{ \tweak control-points #shape-curve #item #})
