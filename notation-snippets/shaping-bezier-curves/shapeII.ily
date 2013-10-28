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
            ;; these are the cpts of just the currently calculated sibling: (?)
            (default-cpts (get-cpts grob))
            (slur-dir (ly:grob-property grob 'direction)))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;
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
       (define (is-rel-polar-spec? x)
         (and (list? x)
              (symbol? (first x))
              (or (eq? 'rp (first x))
                  (eq? 'relative-polar (first x)))))
       ;; position a middle cpt relative to respective outer cpt,
       ;; in polar coordinates.
       (define (polar-coords points spec side relative?)
         (let* ((x-dif (- (car (last points)) (car (first points))))
                (y-dif (- (cdr (last points)) (cdr (first points))))
                (slur-length (sqrt (+ (expt x-dif 2) (expt y-dif 2))))
                (radius (* slur-length (third spec)))
                (ref-slope (if relative? (atan (/ y-dif x-dif)) 0))
                (angl (+ (degrees->radians (second spec))
                        (* side ref-slope slur-dir)))
                (ref-pt (if (= 1 side)
                            (first points)
                            (last points)))
                (x-coord (+ (car ref-pt) (* side radius (cos angl))))
                (y-coord (+ (cdr ref-pt) (* slur-dir radius (sin angl)))))
           (cons x-coord y-coord)))

       ;; end of functions for handling specs. ;;;;;;;;;;;;;;;;;;;
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;

       (define (calc-one-point current-state specs which)
         (if (null? specs)
             (list-ref current-state which)
             (let ((coords (list-ref current-state which))
                   (spec (list-ref specs which))
                   (side (if (< 1 which) -1 1)))
               (cond
                ((is-null-spec? spec) coords)
                ((is-simple-offset-spec? spec)(simple-offset coords spec))
                ((is-smart-offset-spec? spec)(smart-offset coords spec side))
                ((is-absolute-spec? spec)(absolute-coords spec))
                ((is-polar-spec? spec)(polar-coords current-state spec side #f))
                ((is-rel-polar-spec? spec)(polar-coords current-state spec side #t))
                (else (begin
                       (display "Shape error: unknown specification type: ")
                       (display spec)
                       (format #t
                         "\nUsing default coordinates for control-point ~a.\n"
                         (+ which 1))
                       coords))))))

       (define (calc-sibling specs)
         ;; 'specs' is a set of instructions for one sibling.
         (let ((new-cpts default-cpts)
               ;; make \shape #'((foo)) equivalent to \shape #'((foo foo foo foo))
               ;; and \shape #'((foo bar)) to \shape #'((foo bar bar foo)):
               (specs (cond
                       ((= 1 (length specs))
                        (make-list 4 (car specs)))
                       ((= 2 (length specs))
                        (list (first specs)
                          (second specs)
                          (second specs)
                          (first specs)))
                       ((= 3 (length specs))
                        (append specs '(())))
                       (else specs))))

           ;; In some cases (most notably when using polar coordinates),
           ;; middle cpts need to access information that is available
           ;; only after processing outer cpts (e.g. full slur length).
           (list-set! new-cpts 0 (calc-one-point new-cpts specs 0))
           (list-set! new-cpts 3 (calc-one-point new-cpts specs 3))
           (list-set! new-cpts 1 (calc-one-point new-cpts specs 1))
           (list-set! new-cpts 2 (calc-one-point new-cpts specs 2))
           new-cpts))

       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (calc-sibling (car offs))
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
           (calc-sibling (car all-specs)))))

   #{ \tweak control-points #shape-curve #item #})
