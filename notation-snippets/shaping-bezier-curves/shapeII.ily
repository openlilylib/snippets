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

#(define (find-value-to-offset prop self alist)
   "Return the first value of the property @var{prop} in the property
   alist @var{alist} @em{after} having found @var{self}."
(let ((segment (member (cons prop self) alist)))
  (if (not segment)
      (assoc-get prop alist)
      (assoc-get prop (cdr segment)))))

#(define (get-head note-column dir)
   ;; Return the dir-most head from notecolumn.
   ;; This should be implemented in C++ with a Scheme interface.
   (let ((elts (ly:grob-object note-column 'elements))
         (init -inf.0)
         (result #f))
     (for-each
      (lambda (idx)
        (let* ((elt (ly:grob-array-ref elts idx)))
          (if (grob::has-interface elt 'note-head-interface)
              (let ((off (ly:grob-property elt 'Y-offset)))
                (if (> (* off dir) init)
                    (begin
                     (set! init off)
                     (set! result elt)))))))
      (reverse (iota (ly:grob-array-length elts))))
     result))

shapeII =
#(define-music-function (parser location all-specs item)
   (list? symbol-list-or-music?)
   (_i "TODO: write description when finished")
   (define (shape-curve grob)
     (let* ((orig (ly:grob-original grob))
            (siblings (if (ly:spanner? grob)
                          (ly:spanner-broken-into orig) '()))
            (total-found (length siblings))
            (immutable-props (ly:grob-basic-properties grob))
            (value (find-value-to-offset 'control-points shape-curve immutable-props))
            (default-cpts (if (procedure? value)
                              (value grob)
                              value))
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
         (cons (+ (car x)(* -1 i (first y)))
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
       (define (is-abs-polar-spec? x)
         (and (list? x)
              (symbol? (first x))
              (or (eq? 'ap (first x))
                  (eq? 'absolute-polar (first x)))))
       ;; position a middle cpt relative to respective outer cpt,
       ;; in polar coordinates.
       (define (polar-coords points spec side absolute?)
         (let* ((x-dif (- (car (last points)) (car (first points))))
                (y-dif (- (cdr (last points)) (cdr (first points))))
                (slur-length (sqrt (+ (expt x-dif 2) (expt y-dif 2))))
                (radius (* slur-length (second spec)))
                (ref-slope (if absolute? 0 (atan (/ y-dif x-dif))))
                (angl (+ (degrees->radians (third spec))
                        (* -1 side ref-slope slur-dir)))
                (ref-pt (if (= LEFT side)
                            (first points)
                            (last points)))
                (x-coord (- (car ref-pt) (* side radius (cos angl))))
                (y-coord (+ (cdr ref-pt) (* slur-dir radius (sin angl)))))
           (cons x-coord y-coord)))

       (define (is-rel-polar-spec? x)
         (and (list? x)
              (symbol? (first x))
              (or (eq? 'rp (first x))
                  (eq? 'relative-polar (first x)))))
       ;; adjust a middle cpt relative to its default polar-coordinates.
       ;; TODO: merge with the function above?
       (define (rel-polar-coords points spec side)
         (let* ((point1 (if (= LEFT side)
                            (first default-cpts)
                            (last default-cpts)))
                (point2 (if (= LEFT side)
                            (second default-cpts)
                            (third default-cpts)))
                (x-dif (- (car point2) (car point1)))
                (y-dif (- (cdr point2) (cdr point1)))
                (dist (sqrt (+ (expt x-dif 2) (expt y-dif 2))))
                (radius (* dist (second spec)))
                (ref-slope (atan (/ y-dif x-dif)))
                (angl (+ (degrees->radians (third spec))
                        (* -1 side ref-slope slur-dir)))

                (x-coord (- (car point1) (* side radius (cos angl))))
                (y-coord (+ (cdr point1) (* slur-dir radius (sin angl)))))
           (cons x-coord y-coord)))

       (define (is-notehead-spec? x)
         (and (list? x)
              (symbol? (first x))
              (or (eq? 'h (first x))
                  (eq? 'head (first x)))))
       ;; place slur end near the notehead.
       (define (notehead-placement default spec side)
         (let* ((bound (ly:spanner-bound grob side))
                (get-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
                (bound-name (get-name bound)))
           (if (not (eq? bound-name 'NoteColumn))
               default
               (let* ((head (get-head bound slur-dir))
                      (yoff (if (<= 2 (length spec))
                                (third spec)
                                1.2))
                      (xoff (if (<= 3 (length spec))
                                (second spec)
                                0))
                      ;; in case of cross-staff curves:
                      (refp (ly:grob-system grob))
                      (ref-bound (ly:spanner-bound grob LEFT))
                      (ref-y (ly:grob-relative-coordinate ref-bound refp Y))
                      (my-y (ly:grob-relative-coordinate bound refp Y))
                      (cross-staff-correction (- my-y ref-y))
                      ;; UGH!! I have no idea why this is needed, but without this correction
                      ;; the example below renders wrongly:
                      ;; { d''1-\shapeII #'(() (()()()(head))) ( f'' \break a'' g'') }
                      ;; the if clause is necessary because otherwise the 'fix' will
                      ;; break the cross-staff case.  UGH!!
                      (ugh-correction
                       (if (ly:grob-property grob 'cross-staff) ; returns boolean
                           0.0
                           (- (car (ly:grob-property bound 'Y-extent))
                             (car (ly:grob-extent bound refp Y)))))
                      (cross-staff-correction (+ cross-staff-correction ugh-correction))

                      (head-yoff (+ (ly:grob-property head 'Y-offset)
                                   cross-staff-correction))
                      (head-yext (coord-translate
                                  (ly:grob-property head 'Y-extent)
                                  head-yoff))
                      (head-y-mid (+ (* 0.5 (car head-yext))
                                    (* 0.5 (cdr head-yext))))

                      (ref-x (ly:grob-relative-coordinate ref-bound refp X))
                      (head-x (ly:grob-relative-coordinate head refp X))
                      (head-xoff (- head-x ref-x))
                      (head-xext (coord-translate
                                  (ly:grob-property head 'X-extent)
                                  head-xoff))
                      (head-x-mid (+ (* 0.5 (car head-xext))
                                    (* 0.5 (cdr head-xext)))))
                 (cons (+ (* -1 side xoff) head-x-mid)
                   (+ (* slur-dir yoff) head-y-mid))))))

       ;; end of functions for handling specs. ;;;;;;;;;;;;;;;;;;;
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;

       (define (calc-one-point current-state specs which)
         (if (null? specs)
             (list-ref current-state which)
             (let ((coords (list-ref current-state which))
                   (spec (list-ref specs which))
                   (side (if (< 1 which) RIGHT LEFT)))
               (cond
                ((is-null-spec? spec) coords)
                ((is-simple-offset-spec? spec)(simple-offset coords spec))
                ((is-smart-offset-spec? spec)(smart-offset coords spec side))
                ((is-absolute-spec? spec)(absolute-coords spec))
                ((is-polar-spec? spec)(polar-coords current-state spec side #f))
                ((is-abs-polar-spec? spec)(polar-coords current-state spec side #t))
                ((is-rel-polar-spec? spec)(rel-polar-coords current-state spec side))
                ((is-notehead-spec? spec)(notehead-placement coords spec side))
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
