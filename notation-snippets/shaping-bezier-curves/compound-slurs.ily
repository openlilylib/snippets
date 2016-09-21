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
   `((annotate ,boolean? "Boolean" #f)
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
            ;; automatic control points of the non-compound slur
            (orig-cps (ly:slur::calc-control-points grob))

            (cpA (add-points (first orig-cps) (assq-ref options 'start-point)))
            (cpB (add-points (fourth orig-cps) (assq-ref options 'end-point)))

            ;; append the final point as a "virtual inflection"
            (inflections
             (append
              inflections
              (list
               `((point . (1 . 1))
                 (angle . ,(* -1 (assq-ref options 'end-angle)))
                 (ratio-left . ,(assq-ref options 'end-ratio))))))

            ;; data structure holding the new control points,
            ;; a list of lists with four points each,
            ;; the first one being equal to the last of the previous
            (cps
             (if (= (length inflections) 1)
                 ;; If no actual inflections are given simply print the original slur
                 (list orig-cps)
                 (let
                  ;; a cache for dragging information from one inflection to the next
                  ((previous-cps '())
                   (previous-angle 0))
                  (map
                   (lambda (i)
                     (let*
                      (;; inflection objects and points
                        (current-inf (list-ref inflections i))
                        (prev-inf
                         (if (= i 0) #f (list-ref inflections (- i 1))))
                        (prev-pt
                         (if (= i 0) cpA (last previous-cps)))
                        (current-pt
                         (inflection-point cpA cpB (assq-ref current-inf 'point)))

                        ;; zero-based vector between previous and current point
                        (rel-to-prev (sub-points current-pt prev-pt))

                        ;; slope of the line connecting with the previous inflection point
                        (prev-base-angle (ly:angle rel-to-prev))
                        (prev-length (ly:length rel-to-prev))
                        (prev-abs-angle
                         (if (= i 0)
                             (+ prev-base-angle (assq-ref options 'start-angle))
                             previous-angle))
                        (prev-given-angle
                         (if (= i 0)
                             (assq-ref options 'start-angle)
                             (assq-ref prev-inf 'angle)))
                        (absolute-angle (+ prev-base-angle (assq-ref current-inf 'angle)))
                        (prev-ratio-right
                         (if (= i 0)
                             (assq-ref options 'start-ratio)
                             (assq-ref prev-inf 'ratio-right)))
                        (current-cps
                         (list
                          prev-pt
                          (add-points prev-pt
                            (ly:directed
                             prev-abs-angle
                             (* prev-ratio-right prev-length)))
                          (add-points current-pt
                            (ly:directed
                             (+ (+ prev-base-angle (assq-ref current-inf 'angle)) 180)
                             (* (assq-ref current-inf 'ratio-left) prev-length)))
                          current-pt)))
                      (set! previous-cps current-cps)
                      (set! previous-angle absolute-angle)
                      current-cps
                      ))
                   (iota (length inflections))))))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Create actually printable material
            ;;

            ;; Combine slur stencil from all splines
            (slur-stencil
             (apply ly:stencil-add
               (map
                (lambda (spline)
                  (begin
                   (ly:grob-set-property! grob 'control-points spline)
                   (ly:slur::print grob)))
                cps)))

            (original-slur
             (if (assq-ref options 'show-original-slur)
                 (apply
                  ly:stencil-add
                  (list
                   (stencil-with-color
                    (begin
                     (ly:grob-set-property! grob 'control-points orig-cps)
                     (ly:grob-set-property! grob 'layer -1)
                     (ly:slur::print grob))
                    col-bg)
                   (annotate-spline grob orig-cps col-bg)))
                 empty-stencil))

            (spline-annotations
             (if (assq-ref options 'annotate)
                 (apply
                  ly:stencil-add
                  (map
                   (lambda (spline)
                     (annotate-spline grob spline col-new-slur))
                   cps))
                 empty-stencil))

            ) ; end let binding block in "proc" lambda

          ;; Combine slur and optional annotations to final printable stencil
          (ly:stencil-add
           original-slur
           slur-stencil
           spline-annotations
           )) ; end let block in "proc" lambda
         ))
      ) ;; end toplevel let binding block

    #{ \tweak stencil $proc ( #}))

%{

    (let*


     (let
      ((proc
        (lambda (grob)
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