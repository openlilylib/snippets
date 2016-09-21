\version "2.19.48"

%%%%%%%%%%%%%%%%%%%%%%%
% Display Configuration

#(define col-bg (rgb-color .8 .8 .7))
#(define col-orig-slur cyan)
#(define col-new-slur red)
#(define col-grid blue)
#(define col-grid-front magenta)
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

#(define (inflection-point pt1 pt2 ratio)
   "Find a point between two points, giving the X and Y ratio independently"
   (let*
    ((xratio (car ratio))
     (yratio (cdr ratio)))
    (cons
     (+ (* (- 1 xratio) (car pt1)) (* xratio (car pt2)))
     (+ (* (- 1 yratio) (cdr pt1)) (* yratio (cdr pt2))))))

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
   (apply
    ly:stencil-add
     (list
      (connect-dots (first cps) (second cps) col)
      (connect-dots (third cps) (fourth cps) col)
      (connect-dots (fourth cps) (first cps) col)
      (connect-dots (second cps) (third cps)
        (map (lambda (c)
               (/ (+ c 1) 2))
          col))
      (make-cross-stencil (second cps) col)
      (make-cross-stencil (third cps) col))))

#(define (draw-grid pt1 pt2)
   "Draws a reference grid around two corner points.
    Returns a stencil"
   (let*
    ((x-step (/ (- (car pt2) (car pt1)) 10))
     (x-protrude (/ (- (car pt2) (car pt1)) 4.5))
     (y-protrude (* 1.2 (- (cdr pt2) (cdr pt1))))
     (y-center (/ (+ (cdr pt1) (cdr pt2)) 2))
     (y-top (+ (cdr pt2) y-protrude))
     (y-bottom (- (cdr pt1) y-protrude)))
    (apply
     ly:stencil-add
     (append
      ;; draw vertical lines
      (map
       (lambda (i)
         (let ((x (+ (car pt1) (* i x-step))))
         (stencil-with-color
          (ly:stencil-add
           (make-line-stencil
            (if (= 0 (modulo i 5))
                (* 4 grid-thickness)
                grid-thickness)
            x y-bottom x y-top))
          (if (= 0 (modulo i 10))
              col-grid-front
              col-grid))))
       (iota 15 -2))
      (list
       (stencil-with-color
        (ly:stencil-add
         (make-line-stencil
          grid-thickness
          (- (car pt1) x-protrude) y-center (+ (car pt2) x-protrude) y-center))
        col-grid))))))

