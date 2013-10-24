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

#(define (radians->degrees angle-radians)
   "Convert the given angle from degrees to radians."
   (/ angle-radians PI-OVER-180))

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

         ;; In some cases (most notably when using polar coordinates),
         ;; middle cpts need to access information that is available
         ;; only after processing outer cpts (e.g. full slur length).
         (let* ((new-cpts (list #f #f #f #f))
                (slur-length #f)
                (slur-slope #f))

           ;; functions for handling various types of specs: ;;;;;;;;;

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

           (define (is-absolute-spec? x)
             (and (list? x)
                  (symbol? (first x))
                  (or (eq? 'a (first x))
                      (eq? 'abs (first x))
                      (eq? 'absolute (first x)))))
           (define (absolute-coords y)
             (cons (second y)(third y)))

           (define (is-polar-spec? x)
             (and (list? x)
                  (symbol? (first x))
                  (or (eq? 'p (first x))
                      (eq? 'polar (first x)))))
           ;; position a middle cpt relative to respective outer cpt,
           ;; in polar coordinates.
           (define (polar-coords specification idx)
             (let* ((outer-pt (if (= 1 idx)
                                  (first new-cpts)
                                  (last new-cpts)))
                    (angl (degrees->radians (second specification)))
                    (rad (* slur-length (third specification)))
                    (x-coord (+ (car outer-pt) (* idx rad (cos angl))))
                    (y-coord (+ (cdr outer-pt) (* slur-dir rad (sin angl)))))
               (cons x-coord y-coord)))

           ;; end of functions for handling specs. ;;;;;;;;;;;;;;;;;;;

           (define (calc-one-point specs which)
             (if (null? specs)
                 (list-ref default-cpts which)
                 (let* ((spec (list-ref specs which))
                        (dflt (list-ref default-cpts which))
                        (idx (if (< 1 which) -1 1)))
                   (cond ((is-null-spec? spec) dflt) ;; TODO rename idx to side
                     ((is-simple-offset-spec? spec)(simple-offset dflt spec))
                     ((is-smart-offset-spec? spec)(smart-offset dflt spec idx))
                     ((is-absolute-spec? spec)(absolute-coords spec))
                     ((is-polar-spec? spec)(polar-coords spec idx))
                     (else (begin
                            (display "Shape error: unknown specification type: ")
                            (display spec)
                            (display "\nUsing default control-point coordinates.\n")
                            dflt))))))

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

           (list-set! new-cpts 0 (calc-one-point specs 0))
           (list-set! new-cpts 3 (calc-one-point specs 3))

           (let* ((x-dif (- (car (last new-cpts)) (car (first new-cpts))))
                  (y-dif (- (cdr (last new-cpts)) (cdr (first new-cpts)))))
             (set! slur-slope (radians->degrees (atan (/ y-dif x-dif))))
             (set! slur-length (sqrt (+ (expt x-dif 2) (expt y-dif 2)))))

           (list-set! new-cpts 1 (calc-one-point specs 1))
           (list-set! new-cpts 2 (calc-one-point specs 2))

           new-cpts))

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
