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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bezier helper functions taken from scm/bezier-tools.scm

#(define (make-coord x-value y-value)
   "Make a coordinate pair from @var{x-valye} and @var{y-value}."
   (cons x-value y-value))

#(define (coord+ coord1 coord2)
   "Add @var{coord1} to @var{coord2}, returning a coordinate."
   (cons (+ (car coord1) (car coord2))
     (+ (cdr coord1) (cdr coord2))))

#(define (coord- coord1 coord2)
   "Subtract @var{coord2} from @var{coord1}."
   (cons (- (car coord1) (car coord2))
     (- (cdr coord1) (cdr coord2))))

#(define (coord* scalar coord)
   "Multiply each component of @var{coord} by @var{scalar}."
   (cons (* (car coord) scalar)
     (* (cdr coord) scalar)))

#(define (make-bezier point-0 point-1 point-2 point-3)
   "Create a cubic bezier from the four control points."
   (list point-0 point-1 point-2 point-3))

#(define (interpolated-control-points control-points split-value)
   "Interpolate @var{control-points} at @var{split-value}.  Return a
set of control points that is one degree less than @var{control-points}."
   (if (null? (cdr control-points))
       '()
       (let ((first (car control-points))
             (second (cadr control-points)))
         (cons* (coord+ first (coord* split-value (coord- second first)))
           (interpolated-control-points
            (cdr control-points)
            split-value)))))

#(define (split-bezier bezier split-value)
   "Split a cubic bezier defined by @var{bezier} at the value
@var{split-value}.  @var{bezier} is a list of pairs; each pair is
is the coordinates of a control point.  Returns a list of beziers.
The first element is the LHS spline; the second
element is the RHS spline."
   (let* ((quad-points (interpolated-control-points
                        bezier
                        split-value))
          (lin-points (interpolated-control-points
                       quad-points
                       split-value))
          (const-point (interpolated-control-points
                        lin-points
                        split-value))
          (left-side (list (car bezier)
                       (car quad-points)
                       (car lin-points)
                       (car const-point)))
          (right-side (list (car const-point)
                        (list-ref lin-points 1)
                        (list-ref quad-points 2)
                        (list-ref bezier 3))))
     (cons left-side right-side)))

% Bezier helpers
%%%%%%%%%%%%%%%%

#(define (make-line-bezier spline thickness line-thickness)
   "Create a bezier curve drawing a straight line,
    using the thickness and line-thickness from the original slur."
   (let
    ((a (first spline))
     (b (second spline))
     (c (third spline))
     (d (fourth spline)))
    (make-path-stencil
     `(moveto ,(car a) ,(cdr a)
        curveto
        ,(car b) ,(cdr b)
        ,(car c) ,(cdr c)
        ,(car d) ,(cdr d))
     (+ thickness line-thickness)
     1
     1
     #f)))

#(define (make-compound-line-bezier splines thickness line-thickness)
   "Create a list of line bezier segments."
   (map make-line-bezier splines
     (make-list (length splines) thickness)
     (make-list (length splines) line-thickness)))

#(define (make-half-sandwich spline line-thickness thickness dir)
   "Create a half bezier sandwich. The other half is to be added
    as a straight line."
   (let*
    ((cp1 (first spline))
     (cp2 (second spline))
     (cp3 (third spline))
     (cp4 (fourth spline))
     (end-angle (+ 90 (ly:angle (sub-points cp4 cp3))))
     (y (* dir (/ thickness 2)))
     (cp4a (add-points cp4 (ly:directed end-angle y)))
     (cp4b (add-points cp4 (ly:directed end-angle (* -1 y))))
     )
     (make-path-stencil
      `(moveto
        ,(car cp1) ,(cdr cp1)
        curveto
        ; TODO: Is there a better way to create the sandwich
        ; by offsetting the control points (to/from)?
        ,(car cp2) ,(+ (cdr cp2) 0)
        ,(car cp3) ,(+ (cdr cp3) 0)
        ,(car cp4a) ,(cdr cp4a)
        lineto
        ,(car cp4b) ,(cdr cp4b)
        curveto
        ,(car cp3) ,(- (cdr cp3) 0)
        ,(car cp2) ,(- (cdr cp2) 0)
        ,(car cp1) ,(cdr cp1)
        closepath)
      line-thickness
      1
      1
      #t)
    ))

#(define (make-sandwich-opening orig-spline line-thickness thickness)
   "Create an opening sandwich from a half sandwich and a line"
   (let*
    ((split-spline (split-bezier orig-spline 0.5))
     (spline (car split-spline))
     (line-spline (cdr split-spline))
     )
    (ly:stencil-add
     (make-half-sandwich spline line-thickness thickness 1)
     (make-line-bezier line-spline thickness line-thickness)
     )))

#(define (make-sandwich-closing orig-spline line-thickness thickness)
   "Create a closing sandwich from a line and a half sandwich"
   (let*
    ((split-spline (split-bezier orig-spline 0.5))
     (spline (reverse (cdr split-spline)))
     (line-spline (car split-spline))
     )
    (ly:stencil-add
     (make-half-sandwich spline line-thickness thickness -1)
     (make-line-bezier line-spline thickness line-thickness)
     )))
