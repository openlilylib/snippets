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


