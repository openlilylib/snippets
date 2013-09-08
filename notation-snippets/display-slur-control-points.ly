\version "2.16.2"

\header {
  snippet-title = "Displaying control points of slurs and ties"
  snippet-author = "David Nalesnik, Thomas Morley, Urs Liska, Janek Warchoł"
  snippet-description = \markup {
    Slurs, Ties and other similar objects are drawn in LilyPond as
    third-order Bezier curves, which means that their shape is controlled
    by four “control-points” (first and last ones tell where the curve ends
    are placed, and the middle ones affect the curvature).  Changing the
    shape of these objects involves moving these control-points around,
    and it's helpful to see where they actually are.  This snippet defines
    a "\displayControlPoints" function that displays them.  You can use it
    by calling it inside your music expression, or by placing it in
    "\layout" block.  When used inside music expressions, it can be prefixed
    with "\once" in order to display only the control points of the curve that
    starts at this moment.
  }
  % add comma-separated tags to make searching more effective:
  tags = "slur, tie, bezier curve, control point, preview mode"
  % is this snippet ready?  See meta/status-values.md
  status = "buggy" % aiming for status "official"
  %{
    TODO:
    - displaying control-points of ties affects layout! (example at the bottom) FIX!
    - there's an error when trying to use with LaissezVibrerTie - fix!
    - check if this really works with RepeatTies
    - it would be nice to have color controlled by a parameter
    - should we use some global settings for thickness and size?
    the function is called several times for different grobs...
  %}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-cross-stencil coords thickness arm-offset)
   ;; coords are the coordinates of the center of the cross
   (ly:stencil-add
    (make-line-stencil
     thickness
     (- (car coords) arm-offset)
     (- (cdr coords) arm-offset)
     (+ (car coords) arm-offset)
     (+ (cdr coords) arm-offset))
    (make-line-stencil
     thickness
     (- (car coords) arm-offset)
     (+ (cdr coords) arm-offset)
     (+ (car coords) arm-offset)
     (- (cdr coords) arm-offset))))

#(define (display-control-points thickness cross-size)
   ;; both arguments are measured in staff-spaces. Typical values are 0.1 0.5
   (lambda (grob)
     (let* ((grob-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
            (name (grob-name grob))
            (stil (cond ((or (eq? name 'Slur)(eq? name 'PhrasingSlur))(ly:slur::print grob))
                    ((eq? name 'Tie)(ly:tie::print grob))))
            (ctrpts (ly:grob-property grob 'control-points)))

       ;; add crosses:
       (ly:stencil-add stil
         (ly:stencil-in-color
          (ly:stencil-add
           ;; to go from desired cross size (length of line)
           ;; to arm-offset, we have to divide by 2*sqrt(2)
           (make-cross-stencil (second ctrpts) thickness (/ cross-size 2.8284))
           (make-cross-stencil (third ctrpts) thickness (/ cross-size 2.8284))
           )
          1 0 0) ;; color is hard-coded here (R G B).

         ;; add lines:
         (ly:stencil-in-color
          (ly:stencil-add
           (make-line-stencil (/ thickness 4)
             (car (first ctrpts)) (cdr (first ctrpts))
             (car (second ctrpts))  (cdr (second ctrpts)))
           (make-line-stencil (/ thickness 4)
             (car (third ctrpts)) (cdr (third ctrpts))
             (car (fourth ctrpts))  (cdr (fourth ctrpts)))
           )
          1 0 0) ;; color is hard-coded here (R G B).
         empty-stencil)
       )
     ))

% turn on displaying control-points:
displayControlPoints = {
  \override Slur #'stencil = #(display-control-points 0.08 0.4)
  \override PhrasingSlur #'stencil = #(display-control-points 0.08 0.4)
  \override Tie #'stencil = #(display-control-points 0.08 0.4)
  %\override LaissezVibrerTie #'stencil = #(display-control-points 0.08 0.4)
  \override RepeatTie #'stencil = #(display-control-points 0.08 0.4)
}

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%

\layout {
  \displayControlPoints
}

\relative c' {
  c( d e\( d~ d1) g'4 a b f | e\)
}

% this example shows how displayed control-points affect layout, but
% only in case of ties - there are no problems with Slurs and PhrasingSlurs!

\relative c' {
  \override Slur #'stencil = #(display-control-points 0.2 3)
  \override PhrasingSlur #'stencil = #(display-control-points 0.2 3)
  \override Tie #'stencil = #(display-control-points 0.2 3)

  c1~\ppp c a''~^\ppp a
  f,\(\ppp d\) g'\(^\ppp e\)
  f,(\ppp d) g'(^\ppp e)
}
