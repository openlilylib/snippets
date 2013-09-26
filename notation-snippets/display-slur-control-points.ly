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
    - color should be controlled by a parameter
      -> check with ly:stencil-in-color
  %}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example: Configure appearance somewhere in the input files
% before the following definitions are read 
% (i.e. before the snippet is included)
%#(define control-points-line-thickness 0.5)

% Define appearance
#(cond ((not (defined? 'control-points-line-thickness))
        (define control-points-line-thickness 0.05)))
#(cond ((not (defined? 'control-points-cross-thickness))
        (define control-points-cross-thickness 0.1)))
#(cond ((not (defined? 'control-points-cross-size))
        (define control-points-cross-size 0.7)))

#(define (make-cross-stencil coords cross-thickness arm-offset)
   ;; coords are the coordinates of the center of the cross
   (ly:stencil-add
    (make-line-stencil
     cross-thickness
     (- (car coords) arm-offset)
     (- (cdr coords) arm-offset)
     (+ (car coords) arm-offset)
     (+ (cdr coords) arm-offset))
    (make-line-stencil
     cross-thickness
     (- (car coords) arm-offset)
     (+ (cdr coords) arm-offset)
     (+ (car coords) arm-offset)
     (- (cdr coords) arm-offset))))

#(define (display-control-points line-thickness cross-thickness cross-size)
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
           (make-cross-stencil (second ctrpts) cross-thickness (/ cross-size 2.8284))
           (make-cross-stencil (third ctrpts) cross-thickness (/ cross-size 2.8284))
           )
          1 0 0) ;; color is hard-coded here (R G B).

         ;; add lines:
         (ly:stencil-in-color
          (ly:stencil-add
           (make-line-stencil line-thickness
             (car (first ctrpts)) (cdr (first ctrpts))
             (car (second ctrpts))  (cdr (second ctrpts)))
           (make-line-stencil line-thickness
             (car (third ctrpts)) (cdr (third ctrpts))
             (car (fourth ctrpts))  (cdr (fourth ctrpts)))
           )
          1 0 0) ;; color is hard-coded here (R G B).
         empty-stencil)
       )
     ))

% turn on displaying control-points:
displayControlPoints = {
  \override Slur #'stencil = #(display-control-points 
                               control-points-line-thickness
                               control-points-cross-thickness 
                               control-points-cross-size)
  \override PhrasingSlur #'stencil = #(display-control-points 
                               control-points-line-thickness
                               control-points-cross-thickness 
                               control-points-cross-size)
  \override Tie #'stencil = #(display-control-points 
                               control-points-line-thickness
                               control-points-cross-thickness 
                               control-points-cross-size)
  %\override LaissezVibrerTie #'stencil = #(display-control-points 
  %                             control-points-line-thickness
  %                             control-points-cross-thickness 
  %                             control-points-cross-size)
  \override RepeatTie #'stencil = #(display-control-points 
                               control-points-line-thickness
                               control-points-cross-thickness 
                               control-points-cross-size)
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

%{
\relative c' {
  \override Slur #'stencil = #(display-control-points 0.2 0.3 2)
  \override PhrasingSlur #'stencil = #(display-control-points 0.2 0.3 2)
  \override Tie #'stencil = #(display-control-points 0.2 0.3 2)

  c1~\ppp c a''~^\ppp a
  f,\(\ppp d\) g'\(^\ppp e\)
  f,(\ppp d) g'(^\ppp e)
}
%}
