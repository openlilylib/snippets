\version "2.19.48"

%%%%%%%%%%%%%%%%%%%%%%%
% Display Configuration

#(define col-bg (rgb-color .8 .8 .7))
#(define col-orig-slur cyan)
#(define col-slur1 '(1 .3 0))
#(define col-slur2 '(1 0 .3))
#(define col-grid blue)
#(define conn-thickness 0.05)
#(define cross-thickness 0.1)
#(define cross-size 0.2)
#(define grid-thickness 0.05)

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geometry helper functions

#(define (add-points pt1 pt2)
   "Add two points"
   (cons (+ (car pt1) (car pt2))
     (+ (cdr pt1) (cdr pt2))))

#(define (sub-points pt1 pt2)
   (cons (- (car pt1) (car pt2))
     (- (cdr pt1) (cdr pt2))))

#(define (inflection-point pt1 pt2 x-ratio y-offset)
   "Find a point between two points, giving the X and Y ratio independently"
   (cons
    (+ (* (- 1 x-ratio) (car pt1)) (* x-ratio (car pt2)))
    (+ (/ (+ (cdr pt1) (cdr pt2)) 2) y-offset)))

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


#(define (annotate-spline grob cps col)
   "Print crosses and a boundary trapezoid for a spline.
    Returns a stencil"
   (let*
    ((baseline-length (ly:length (sub-points (fourth cps) (first cps))))
     (cp2a
      (add-points (first cps)
        (ly:directed
         (ly:angle (sub-points (second cps) (first cps)))
         baseline-length)))
     (cp3a
      (add-points (fourth cps)
        (ly:directed
         (ly:angle (sub-points (third cps) (fourth cps)))
         baseline-length)))
     )
   (apply
    ly:stencil-add
     (list
      (connect-dots (first cps) (second cps) col)
      (stencil-with-color
       (ly:stencil-add
      (ly:line-interface::line grob
        (car (second cps))
        (cdr (second cps))
        (car cp2a) (cdr cp2a))
            (ly:line-interface::line grob
        (car (third cps))
        (cdr (third cps))
        (car cp3a) (cdr cp3a)))
       col-bg)
      (make-cross-stencil cp2a col-bg)
      (make-cross-stencil cp3a col-bg)

      (connect-dots (third cps) (fourth cps) col)
      (connect-dots (fourth cps) (first cps) col)
      (connect-dots (second cps) (third cps)
        (map (lambda (c)
               (/ (+ c 1) 2))
          col))
      (make-cross-stencil (second cps) col)
      (make-cross-stencil (third cps) col)))))

#(define (draw-grid grob pt1 pt2)
   "Draws a reference grid around two corner points.
    Returns a stencil"
   (let*
    ((x-step (/ (- (car pt2) (car pt1)) 10))
     (x-protrude 2.5)
     (y-protrude (max (* 1.2 (- (cdr pt2) (cdr pt1))) 3))
     (y-center (/ (+ (cdr pt1) (cdr pt2)) 2))
     (y-top (+ (cdr pt2) y-protrude))
     (y-bottom (- (cdr pt1) y-protrude)))
    (stencil-with-color
     (ly:stencil-add
      (apply
       ly:stencil-add
       (append
        ;; draw vertical lines every 1/10th
        (map
         (lambda (i)
           (let ((x (+ (car pt1) (* i x-step))))
             (ly:stencil-add
              (make-line-stencil
               (if (= 0 (modulo i 5))
                   (* 4 grid-thickness)
                   grid-thickness)
               x y-bottom x y-top)
              (ly:stencil-translate
               (ly:stencil-scale
                (ly:stencil-add
                 (grob-interpret-markup grob
                   (number->string (exact->inexact (/ i 10)))))
                0.8 0.8)
               (cons (- x 1) (+ y-top 0.7)))
              )))
         (iota 11))

        ;; draw horizontal line through center
        (list
         (make-line-stencil
          (* 2 grid-thickness)
          (- (car pt1) x-protrude) y-center (+ (car pt2) x-protrude) y-center))
        ;; draw horizontal guides every 10 staff spaces,
        ;; add staff space indicators
        (map
         (lambda (i)
           (ly:stencil-add
            (make-line-stencil
             grid-thickness
             (- (car pt1) x-protrude)
             (+ y-center (* i 10))
             (+ (car pt2) x-protrude)
             (+ y-center (* i 10)))
            (apply ly:stencil-add
              (map (lambda (x)
                     (ly:stencil-translate
                      (ly:stencil-scale
                       (ly:stencil-add
                        (grob-interpret-markup grob
                          (number->string (* i 10))))
                       0.8 0.8)
                      (cons x (+ -0.5 y-center (* i 10)))))
                (list -5 (+ (car pt2) 3))))))
         '(-2 -1 1 2)))))
     col-grid)))
