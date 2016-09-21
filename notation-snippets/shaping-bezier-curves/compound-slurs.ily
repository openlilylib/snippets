\version "2.19.48"

\include "common-math-and-stencils.ily"

% Predicates and defaults for inflection properties
% TODO:
% Move to oll-core options
#(define inflection-rules
   `((point ,number-pair? (.5 . .5) "pair of numbers (ratio)")
     (angle ,number? -90 "number (0-360)")
     (ratio-left ,number? .25 "number (0-1)")
     (ratio-right ,number? .25 "number (0-1")))

#(define option-rules
   `((show-control-points ,boolean? "Boolean" #f)
     (show-original-slur ,boolean? "Boolean" #f)
     (offsets ,list? "List of four number pairs" ((0 . 0)(0 . 0)(0 . 0)(0 . 0)))
     (start-point ,number-pair? "pair of numbers" (0 . 0))
     (start-angle ,number? "number?" 90)
     (start-ratio ,number? "number?" 0.5)
     (end-point ,number-pair? "pair of numbers" (0 . 0))
     (end-angle ,number? "number?" 90)
     (end-ratio ,number? "number?" 0.5)
     ))

compoundSlur =
#(define-event-function (options)(ly:context-mod?)
   (let*
    (;; location is needed to refer to the *original* location from further within
      (location (*location*))
      (options
       (map (lambda (o) (cons (second o) (third o)))
         (ly:get-context-mods options)))

      ;; Determine the *given* properties of the inflection points.
      ;; If none are specified this will return an empty list
      (inflections
       (let*
        ((result '())
         ;; known items for an inflection, with
         ;; predicate, fallback value and error message
         ;
         ; TODO:
         ; change to oll-core (getOption)
         ;
         (rules inflection-rules)

         ;; type check and defaulting for inflections
         ;; returns a valid inflection or an empty list
         (check-inflection
          (lambda (i)
            (if (and (list? i)
                     (every pair? i))
                (begin
                 (let*
                  ((pred? (lambda (obj)
                            (if (member obj (map car rules))
                                #f #t)))
                   (unknown-items (filter pred? (map car i))))
                  (if (not (null? unknown-items))
                      (ly:input-warning location "Unknown items in inflection: are discarded. ~a" unknown-items)))
                 (list
                  (map
                   (lambda (rule)
                     (let ((item (assq (car rule) i)))
                       (if item
                           ;; there is an item for the current rule
                           (if ((second rule) (cdr item))
                               item
                               ;; type check error
                               (begin
                                (ly:input-warning location
                                  "
\\compoundSlur: Type check failed with inflection.
Item ~a must be of type ~a. Used default value ~a"
                                  (car item) (fourth rule) (third rule))
                                (cons (first rule) (third rule))))
                           ;; missing value
                           (cons (first rule) (third rule)))
                       ))
                   rules)))
                (begin
                 (ly:input-warning location "
Type check failed. Inflection must be an association list.
Ignore this inflection point")
                 '())))))
        (for-each
         (lambda (opt)
           (if (eq? (car opt) 'inflection)
               (set! result (append result (check-inflection (cdr opt))))))
         options)

        ;; remove inflections from option list
        (set! options
              (let ((no-inflection?
                     (lambda (o)
                       (if (eq? (car o) 'inflection)
                           #f #t))))
                (filter no-inflection? options)))

        ;; return (potentially empty) list of inflections
        result)) ;; end inflections

      ;; Clean up options, doing type checking and defaulting
      (options
       (map
        (lambda (rule)
          (let*
           ((name (first rule))
            (default (fourth rule))
            (opt (assq name options)))
           (if opt
               (if ((second rule) (cdr opt))
                   opt
                   (begin
                    (ly:input-warning location "
\\compoundSlur: wrong type for option \"~a\".
Expected ~a, using default \"~a\"." name (third rule) default)
                    (cons name default)))
               (cons name default))))
        option-rules))

      (proc
       (lambda (grob)
         (let*
          (

            ) ; end let binding block in "proc" lambda
         red ; return value
         ) ; end let block in "proc" lambda
         ))

      ) ;; end toplevel let binding block

    (pretty-print options)
    (pretty-print inflections)

    #{ \tweak color $proc ( #}

    ) ; end outermost let block
   ) % end \compoundSlur

%{

    (let*


     (let
      ((proc
        (lambda (grob)
          (let*
           ((opts (check-options options))
            ;;
            ;; Retrieve options and set defaults
            ;;
            ;; Configuration
            (show-control-points (assq-ref opts 'show-control-points))
            (show-original-slur (assq-ref opts 'show-original-slur))

            ;; Define start and end of overall slur
            (start-point (assq-ref opts 'start-point))
            (start-angle (assq-ref opts 'start-angle))
            (start-ratio (assq-ref opts 'start-ratio))
            (end-point (assq-ref opts 'end-point))
            (end-angle (assq-ref opts 'end-angle))
            (end-ratio (assq-ref opts 'end-ratio))

            ; deprecated, won't be needed anymore when points are
            ; calculated in polar coordinates
            (offsets (assq-ref opts 'offsets))

            ;; automatic control points of the non-compound slur
            (orig-cps (ly:slur::calc-control-points grob))

            ;; data structure holding the new control points,
            ;; a list of lists with four points each,
            ;; the first one being equal to the last of the previous
            (cps '())

            ;; add offsets to the four control points
            ; deprecated
            (cpA (add-points (first orig-cps) start-point))
            (cpA1 (add-points (second orig-cps) (second offsets)))
            (cpB1 (add-points (third orig-cps) (third offsets)))
            (cpB (add-points (fourth orig-cps) end-point))

            (new-inflections (process-inflections opts))


            ;; prepare data structure to handle inflection points
            ;; chain beginning, all given inflections and end to one list
            (basic-inflects
             (append
              (list
              `((point . ,cpA)
                (given-angle . ,(assq-ref opts 'start-angle))
                (ratio-right . ,(assq-ref opts 'start-ratio))))
              (map
              (lambda (i)
                `((point . ,(inflection-point cpA cpB (assq-ref i 'point)))
                  (given-angle . ,(assq-ref i 'angle))
                  (ratio-left . ,(assq-ref i 'ratio-left))
                  (ratio-right . ,(assq-ref i 'ratio-right))))
              (check-inflection-defaults (assq-ref opts 'inflections)))
              (list `((point . ,cpB)
                (given-angle . ,(* -1 (assq-ref opts 'end-angle)))
                (ratio-left . ,(assq-ref opts 'end-ratio))))))

            ;; now calculate remaining properties that depend on the other points
            (inflects
             (let
              ((index 0)
               (max-index (- (length basic-inflects) 1)))
;              (ly:message "basic-inflects: ~a" basic-inflects)
              (map
               (lambda (i)
;                 (ly:message "Index: ~a" index)
;                 (ly:message "Point: ~a" (assq-ref i 'point))
                 (if (and
                      (<= index max-index)
                      (> index 0))
                     (let*
                      ((previous-inf (list-ref basic-inflects (- index 1)))
                       (pt (assq-ref i 'point))
                       (previous-pt (assq-ref previous-inf 'point))
                       (baseline-angle
                        (ly:angle
                         (sub-points pt previous-pt)))
                       (baseline-length
                        (ly:length (sub-points pt previous-pt)))
                       (previous-absolute-angle
                        (if (= index 1)
                            (+ baseline-angle (assq-ref previous-inf 'given-angle))
                            (assq-ref previous-inf 'absolute-angle)))
;                       (previous-cp2
;                        (add-points
;                         previous-pt
;                         (ly:directed previous-absolute-angle
;                           (* baseline-length (assq-ref previous-inf 'ratio-right)))))
;                       (previous-cp3
;                        (add-points pt
;                          (ly:directed previous-absolute-angle
;                            (* baseline-length (assq-ref i 'ratio-left)))))
                       )
                      `((point . ,pt)
                        (baseline-angle . ,baseline-angle)
                        (baseline-length . ,baseline-length)
;                        (previous-cp2 . ,previous-cp2)
;                        (previous-cp3 . ,previous-cp3)
                        )
                     i)

                     (let
                      ((pt (assq-ref i 'point))
                       (next-pt (assq-ref (list-ref basic-inflects (+ 1 index)) 'point)))
                      (set!
                       i (assq-set! i
                           'length (distance pt next-pt)))
                     (set!
                      i (assq-set! i
                          'baseline-angle
                          (ly:angle (sub-points next-pt pt))))
                     ))
;                 (ly:message "inflect: ~a" i)
                 (set! index (+ 1 index))
                 i)
               basic-inflects)
              )
             )

            ;; in an initial run we only get the point and the options
            (inflections
             (map
              (lambda (i)
                `((opts . ,i)
                  (point . ,(inflection-point cpA cpB (assq-ref i 'point)))))
              (check-inflection-defaults (assq-ref opts 'inflections))))


            (base-angle (ly:angle (sub-points cpB cpA)))
            )


           (define (previous-inflection-point index)
             (cond
              ((= index 1) cpA)
              ((= (- (length inflections) 1)) cpB)))

           (ly:message "New inflections: ~a" new-inflections)

           (let*
            ;; Determine the remaining properties of the inflections
            ((inflections
              (let ((index 0)
                    (high-index (length inflections)))
                (map
                 (lambda (i)
                   (set! index (+ 1 index))
                   (let*
                    ((pt (assq-ref i 'point))
                     (previous-pt (previous-inflection-point index))
                     (previous-base-angle (ly:angle (sub-points pt previous-pt)))
                     )
                    `((opts . ,(assq-ref i 'opts))
                      (point . ,pt)
                      (angle . ,(+ previous-base-angle (get-from-inflection i 'angle)))
                      (previous-pt . ,previous-pt)
                      (previous-length . ,(distance pt previous-pt))
                      )
                    )
                   )
                 inflections)))

             ; TODO: This is only valid for a single inflection
             (inflection (first inflections))

             ;; calculate inflection point and surrounding control points
             (cp4 (inflection-point cpA cpB (get-from-inflection inflection 'point)))
             ;; left hand side length of the inflection
             ;; (if given it is the ratio to the left "baseline"
             ;; otherwise it is the same length as the leftmost control point distance)
             (cp3 (add-points cp4
                    (ly:directed  (assq-ref inflection 'angle)
                      (if (get-from-inflection inflection 'ratio-left)
                          (* -1 (get-from-inflection inflection 'ratio-left) (distance cpA cp4))
                          (* -1 (distance cpA cpA1))))))
             ;; right hand side length of the inflection
             (cp5 (add-points cp4
                    (ly:directed (assq-ref inflection 'angle)
                      (if (get-from-inflection inflection 'ratio-right)
                          (* (get-from-inflection inflection 'ratio-right) (distance cp4 cpB))
                          (distance cpB cpB1)))))

             (first-spline-stil
              (begin
               (ly:grob-set-property! grob 'control-points (list cpA cpA1 cp3 cp4))
               (ly:slur::print grob)))
             (second-spline-stil
              (begin
               (ly:grob-set-property! grob 'control-points (list cp4 cp5 cpB1 cpB))
               (ly:slur::print grob)))
             ;; display original slur and its control points
             (original-slur
              (if show-original-slur
                  (apply
                   ly:stencil-add
                   ;; display control points of the original, non-compound slur
                   (append
                    ;; display original slur
                    (list
                     (stencil-with-color
                      (begin
                       (ly:grob-set-property! grob 'control-points orig-cps)
                       (ly:grob-set-property! grob 'layer -1)
                       (ly:slur::print grob))
                      col-bg))
                    (list
                     (connect-dots cpA cpB col-bg))
                    ;; display obsolete handles of the original slur
                    (map
                     (lambda (c1 c2)
                       (connect-dots c1 c2 col-bg))
                     (list (first orig-cps) (fourth orig-cps))
                     (list (second orig-cps) (third orig-cps)))
                    (map
                     (lambda (c)
                       (make-cross-stencil c col-orig-slur))
                     orig-cps)
                    ))
                  empty-stencil))
             ;; display new control-points and connections
             (crosses
              (if show-control-points
                  (apply
                   ly:stencil-add
                   (append
                    ;; display actual control points of the compound slur
                    (map
                     (lambda (c)
                       (make-cross-stencil c col-new-slur))
                     (list cpA cpA1 cp3 cp4 cp5 cpB1 cpB))
                    ;; display connections between original and offset control points
                    (map
                     (lambda (c1 c2)
                       (connect-dots c1 c2 col-orig-slur))
                     orig-cps
                     (list cpA cpA1 cpB1 cpB))
                    ;; display handles indicating the
                    (map
                     (lambda (c1 c2)
                       (connect-dots c1 c2 col-new-slur))
                     (list cpA  cp3 cp4 cpB  cpA)
                     (list cpA1 cp4 cp5 cpB1 cp4))
                    ))
                  empty-stencil))
             )
            (ly:message "Length of previous segment: ~a" (assq-ref inflection 'previous-length))

            (ly:stencil-add
             original-slur
             first-spline-stil
             second-spline-stil
             crosses
             ))))))
      #{ -\tweak stencil $proc ( #}))))

%}