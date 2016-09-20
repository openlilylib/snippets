\version "2.19.48"

\include "common-math-and-stencils.ily"

compoundSlur =
#(define-event-function (options)(ly:context-mod?)
   (let
    ((inflection-rules
      `((point ,number-pair? (.5 . .5))
        (angle ,number? -90)
        (ratio-left ,number? .25)
        (ratio-right ,number? .25))))

    (define (inflection? obj)
      (let
       ((rules
         (map (lambda (r)
                (cons (first r) (second r)))
           inflection-rules)))
       (and (list? obj)
            (every
             (lambda (o)
               (and (pair? o)
                    (member (car o) (map car inflection-rules))
                    ((assq-ref rules (car o)) (cdr o))))
             obj))))

    (let*
     ((location (*location*))
      (option-rules
       `((show-control-points ,boolean? "Boolean" #f)
         (show-original-slur ,boolean? "Boolean" #f)
         (offsets ,list? "List of four number pairs" ((0 . 0)(0 . 0)(0 . 0)(0 . 0)))
         (inflection ,inflection? "alist with allowed keys
- point (number-list?)
- angle (number?)
- ratio-left (number?) and
- ratio-right (number?)"
           ,(map (lambda (r)
                   (cons (first r) (third r)))
              inflection-rules)
           ))))

     (define (check-options args)
       (let
        ((options
          (map
           (lambda (o)
             (cons (second o) (third o)))
           (ly:get-context-mods args))))
        (map
         (lambda (rule)

           (let*
            ((name (first rule))
             (pred (second rule))
             (default (fourth rule))
             (opt (assq name options))
             )
            (if opt
                (begin
                 (if (pred (cdr opt))
                     opt
                     (begin
                      (ly:input-warning location "
\\compoundSlur: wrong type for option \"~a\".
Expected ~a, using default \"~a\"." name (third rule) default)
                      (cons name default))))
                (cons name default))))
         option-rules)))

     (define (check-inflection-defaults inflection)
       (map
        (lambda (i)
          (let ((prop (assq (car i) inflection)))
            (ly:message "prop: ~a" prop)
            (if prop
                prop
                (cons (first i) (third i)))))
        inflection-rules))

     (let
      ((proc
        (lambda (grob)
          (let*
           ((opts (check-options options))
            ;; Retrieve options and set defaults
            (show-control-points (assq-ref opts 'show-control-points))
            (show-original-slur (assq-ref opts 'show-original-slur))
            (offsets (assq-ref opts 'offsets))
            (inflection (check-inflection-defaults (assq-ref opts 'inflection)))
            (inflection-ratio (assq-ref inflection 'point))
            (inflection-angle (assq-ref inflection 'angle))
            (inflection-ratio-left (assq-ref inflection 'ratio-left))
            (inflection-ratio-right (assq-ref inflection 'ratio-right))

            ;; automatic control points of the non-compound slur
            (cps (ly:slur::calc-control-points grob))

            ;; add offsets to the four control points
            (cp1 (add-points (first cps) (first offsets)))
            (cp2 (add-points (second cps) (second offsets)))
            (cp6 (add-points (third cps) (third offsets)))
            (cp7 (add-points (fourth cps) (fourth offsets)))

            (base-angle (ly:angle (sub-points cp7 cp1)))

            ;; calculate inflection point and surrounding control points
            (cp4 (inflection-point cp1 cp7 inflection-ratio))
            ;; left hand side length of the inflection
            ;; (if given it is the ratio to the left "baseline"
            ;; otherwise it is the same length as the leftmost control point distance)
            (cp3 (add-points cp4
                   (ly:directed (+ base-angle inflection-angle)
                     (if inflection-ratio-left
                         (* -1 inflection-ratio-left (distance cp1 cp4))
                         (* -1 (distance cp1 cp2))))))
            ;; right hand side length of the inflection
            (cp5 (add-points cp4
                   (ly:directed (+ base-angle inflection-angle)
                     (if inflection-ratio-right
                         (* inflection-ratio-right (distance cp4 cp7))
                         (distance cp7 cp6)))))

            ;                 (sloped-point
            ;                  inflection-slope
            ;                  (if inflection-ratio-right
            ;                      (* inflection-ratio-right (distance cp4 cp7))
            ;                      (distance cp7 cp6)))))

            (first-spline-stil
             (begin
              (ly:grob-set-property! grob 'control-points (list cp1 cp2 cp3 cp4))
              (ly:slur::print grob)))
            (second-spline-stil
             (begin
              (ly:grob-set-property! grob 'control-points (list cp4 cp5 cp6 cp7))
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
                      (ly:grob-set-property! grob 'control-points cps)
                      (ly:grob-set-property! grob 'layer -1)
                      (ly:slur::print grob))
                     col-bg))
                   (list
                    (connect-dots cp1 cp7 col-bg))
                   ;; display obsolete handles of the original slur
                   (map
                    (lambda (c1 c2)
                      (connect-dots c1 c2 col-bg))
                    (list (first cps) (fourth cps))
                    (list (second cps) (third cps)))
                   (map
                    (lambda (c)
                      (make-cross-stencil c col-orig-slur))
                    cps)
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
                    (list cp1 cp2 cp3 cp4 cp5 cp6 cp7))
                   ;; display connections between original and offset control points
                   (map
                    (lambda (c1 c2)
                      (connect-dots c1 c2 col-orig-slur))
                    cps
                    (list cp1 cp2 cp6 cp7))
                   ;; display handles indicating the
                   (map
                    (lambda (c1 c2)
                      (connect-dots c1 c2 col-new-slur))
                    (list cp1 cp3 cp4 cp7)
                    (list cp2 cp4 cp5 cp6))
                   ))
                 empty-stencil))
            )
           (ly:message "base angle: ~a" base-angle)

           (ly:stencil-add
            original-slur
            first-spline-stil
            second-spline-stil
            crosses
            )))))
      #{ -\tweak stencil $proc ( #}))))

