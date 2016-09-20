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

    (define (inflection-list? obj)
      (and (list? obj)
           (every inflection? obj)))

    (let*
     ((location (*location*))
      (option-rules
       `((show-control-points ,boolean? "Boolean" #f)
         (show-original-slur ,boolean? "Boolean" #f)
         (offsets ,list? "List of four number pairs" ((0 . 0)(0 . 0)(0 . 0)(0 . 0)))
         (inflections ,inflection-list? "alist with allowed keys
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

     (define (check-inflection-defaults inflections)
       (map
        (lambda (i)
          (map
           (lambda (r)
             (let ((opt (assq (car r) i)))
               (if opt opt
                   (cons (first r)(third r)))))
           inflection-rules))
        inflections))

     (define (get-from-inflection inflection name)
       (ly:message "inf: ~a" inflection)
       (let ((opts (assq-ref inflection 'opts)))
         (assq-ref opts name)))
     

     (let
      ((proc
        (lambda (grob)
          (let*
           ((opts (check-options options))
            ;; Retrieve options and set defaults
            (show-control-points (assq-ref opts 'show-control-points))
            (show-original-slur (assq-ref opts 'show-original-slur))
            (offsets (assq-ref opts 'offsets))

            ;; automatic control points of the non-compound slur
            (orig-cps (ly:slur::calc-control-points grob))

            ;; add offsets to the four control points
            (cpA (add-points (first orig-cps) (first offsets)))
            (cpA1 (add-points (second orig-cps) (second offsets)))
            (cpB1 (add-points (third orig-cps) (third offsets)))
            (cpB (add-points (fourth orig-cps) (fourth offsets)))

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

