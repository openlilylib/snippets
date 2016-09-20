\version "2.19.48"

%%%%%%%%%%%%%%%%%%%%%%%
% Display Configuration

#(define col-bg (rgb-color .8 .8 .7))
#(define col-orig-slur cyan)
#(define col-new-slur red)
#(define conn-thickness 0.05)
#(define cross-thickness 0.1)
#(define cross-size 0.2)

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geometry helper functions

#(define (mirror-point pt1 pt2)
   "Mirror pt2 against pt1"
   (cons
    (- (* 2 (car pt1)) (car pt2))
    (- (* 2 (cdr pt1)) (cdr pt2))))

#(define (add-points pt1 pt2)
   "Add two points"
   (cons (+ (car pt1) (car pt2))
     (+ (cdr pt1) (cdr pt2))))

#(define (sub-points pt1 pt2)
   (cons (- (car pt1) (car pt2))
     (- (cdr pt1) (cdr pt2))))

#(define (sloped-point slope dist)
   "Create a point with given slope and distance"
   (let
    ; TODO: Is this too confused, can it be done inline?
    ((factor (* dist (/ 1 (distance '(0 . 0) slope )))))
   (cons
     (* (cdr slope) factor)
     (* (car slope) factor))))

#(define (inflection-point pt1 pt2 ratio)
   "Find a point between two points, giving the X and Y ratio independently"
   (let*
    ((xratio (car ratio))
     (yratio (cdr ratio)))
    (cons
     (+ (* (- 1 xratio) (car pt1)) (* xratio (car pt2)))
     (+ (* (- 1 yratio) (cdr pt1)) (* yratio (cdr pt2))))))

#(define (distance pt1 pt2)
   "Caculate distance between two points"
   (ly:length (sub-points pt1 pt2)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Control point visualization

#(define (make-cross-stencil coord col)
   "Draw a cross-stencil at coord."
   (stencil-with-color
    (ly:stencil-add
     (make-line-stencil
      cross-thickness
      (- (car coord) cross-size)
      (- (cdr coord) cross-size)
      (+ (car coord) cross-size)
      (+ (cdr coord) cross-size))
     (make-line-stencil
      cross-thickness
      (- (car coord) cross-size)
      (+ (cdr coord) cross-size)
      (+ (car coord) cross-size)
      (- (cdr coord) cross-size)))
    col))

#(define (connect-dots pt1 pt2 col)
   "Draw a thin line connecting two points"
   (stencil-with-color
    (ly:stencil-add
     (make-line-stencil
      conn-thickness
      (car pt1)
      (cdr pt1)
      (car pt2)
      (cdr pt2)))
    col))


#(define (default-option opts name value)
   "Retrieve an option value or a default"
   (let ((prop (assq name opts)))
     (if prop (cdr prop) value)))

compoundSlur =
#(define-event-function (options)(ly:context-mod?)
   ; TODO: Change inner-segment to
   ; - a ratio (to the length of the line between cp1 and cp4)
   ; - an angle (relative to the same line cp1--cp2)
   (let
    ((proc
      (lambda (grob)
        (let*
         ((opts
           (map
            (lambda (o)
              (cons (second o) (third o)))
            (ly:get-context-mods options)))
          ;; Retrieve options and set defaults
          (offsets
           (default-option opts 'offsets '((0 . 0)(0 . 0)(0 . 0)(0 . 0))))
          (inflection-ratio
           (default-option opts 'inflection-ratio '(.5 . .5)))
          (inflection-angle
           (default-option opts 'inflection-angle 0))
          (ann?
           (default-option opts 'ann #f))

          (inflection-ratio-left
           (default-option opts 'inflection-ratio-left #f))
          (inflection-ratio-right
           (default-option opts 'inflection-ratio-right #f))
          ; TODO:
          (inner-segment (assq-ref opts 'inner-segment))

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
           (if ann?
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
                 (map
                  (lambda (c)
                    (make-cross-stencil c col-orig-slur))
                  cps)
                 ))
               empty-stencil))
          ;; display new control-points and connections
          (crosses
           (if ann?
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
                 ;; display obsolete handles of the original slur
                 (map
                  (lambda (c1 c2)
                    (connect-dots c1 c2 col-bg))
                  (list (first cps) (fourth cps))
                  (list (second cps) (third cps)))
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
    #{ -\tweak stencil $proc ( #}))

