\version "2.16.2"

\header {
  snippet-title = "Displaying control points of bezier curves"
  snippet-author = "David Nalesnik, Thomas Morley, Urs Liska, Janek Warchoł"
  snippet-description = \markup {
    Slurs, Ties and other similar objects are drawn in LilyPond as
    third-order Bezier curves, which means that their shape is controlled
    by four "control-points" (first and last ones tell where the curve ends
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
    - displaying control-points of ties affects layout in version 2.17!
      (Which version introduces that change?)
      Workaround is provided in the examples.
  %}
  %% All done or worked around
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
#(cond ((not (defined? 'control-points-cross-color))
        (define control-points-cross-color red)))
#(cond ((not (defined? 'control-points-line-color))
        (define control-points-line-color blue)))

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

#(define (display-control-points 
          line-thickness 
          line-color 
          cross-thickness 
          cross-size 
          cross-color)
   ;; 'cross-thickness' and 'cross-size' are measured in staff-spaces.
   ;;  Typical values are 0.1 0.5
   ;; 'line-color' and 'cross-color' takes rgb-colors, x11-colors or the
   ;; predefined colors like red, green etc
   (lambda (grob)
     (let* ((grob-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
            (name (grob-name grob))
            (stil (cond ((or (eq? name 'Slur)
                             (eq? name 'PhrasingSlur))
                         (ly:slur::print grob))
                        ((eq? name 'LaissezVibrerTie)
                         (laissez-vibrer::print grob))
                        ((or (eq? name 'Tie)
                             (eq? name 'RepeatTie))
                         (ly:tie::print grob))))
            (ctrpts (ly:grob-property grob 'control-points))
            (cross-stencils
              (ly:stencil-add
                ;; to go from desired cross size (length of line)
                ;; to arm-offset, we have to divide by 2*sqrt(2)
                ;;
                ;; If you want to see the first and the last control-point, too,
                ;; uncomment the relevant lines.
                ;; TODO: Execute the statment when 'control-points-display-first is defined
                ;(make-cross-stencil (first ctrpts) cross-thickness (/ cross-size 2.8284))
                (make-cross-stencil (second ctrpts) cross-thickness (/ cross-size 2.8284))
                (make-cross-stencil (third ctrpts) cross-thickness (/ cross-size 2.8284))
                ;; TODO: Execute the statment when 'control-points-display-fourth is defined
                ;(make-cross-stencil (fourth ctrpts) cross-thickness (/ cross-size 2.8284))
                ))
            (line-stencils
               (ly:stencil-add
                (make-line-stencil line-thickness
                  (car (first ctrpts)) (cdr (first ctrpts))
                  (car (second ctrpts))  (cdr (second ctrpts)))
                ;; If you want a line from second to third control-point uncomment
                ;; the following expression.
                ;; TODO: Execute the statment when 'control-points-display-middle-line is defined
                ;(make-line-stencil line-thickness
                ;  (car (second ctrpts)) (cdr (second ctrpts))
                ;  (car (third ctrpts))  (cdr (third ctrpts)))
                (make-line-stencil line-thickness
                  (car (third ctrpts)) (cdr (third ctrpts))
                  (car (fourth ctrpts))  (cdr (fourth ctrpts)))
                ))
            )

       ;; The order of adding the stencils will determine which stencil is printed
       ;; below or above, similar to 'layer
       ;; TODO: Is there consensus about it?
       ;;
       ;; Setting the added stencils to empty extents solves the tie-issue for 2.16.2
       ;; but not for 2.17.x
       ;; I think there's still something fishy with the skyline-code.
       ;; Workaround: add \override Tie #'vertical-skylines = #'()
       ;; as shown in the last example below.
       (ly:stencil-add stil
        ;; add crosses:
        (ly:make-stencil
         (ly:stencil-expr (stencil-with-color cross-stencils cross-color))
         empty-interval
         empty-interval)

         ;; add lines:
        (ly:make-stencil
         (ly:stencil-expr (stencil-with-color line-stencils line-color))
         empty-interval
         empty-interval)
         empty-stencil)
       )
     ))

% turn on displaying control-points:
displayControlPoints = {
  \override Slur #'stencil = #(display-control-points
                               control-points-line-thickness
                               control-points-line-color
                               control-points-cross-thickness
                               control-points-cross-size
                               control-points-cross-color)
  \override PhrasingSlur #'stencil = #(display-control-points
                               control-points-line-thickness
                               control-points-line-color
                               control-points-cross-thickness
                               control-points-cross-size
                               control-points-cross-color)
  \override Tie #'stencil = #(display-control-points
                               control-points-line-thickness
                               control-points-line-color
                               control-points-cross-thickness
                               control-points-cross-size
                               control-points-cross-color)
  \override LaissezVibrerTie #'stencil = #(display-control-points
                               control-points-line-thickness
                               control-points-line-color
                               control-points-cross-thickness
                               control-points-cross-size
                                control-points-cross-color)
  \override RepeatTie #'stencil = #(display-control-points
                               control-points-line-thickness
                               control-points-line-color
                               control-points-cross-thickness
                               control-points-cross-size
                               control-points-cross-color)
}

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%
%#(ly:set-option 'debug-skylines)

\layout {
  \displayControlPoints
}

\relative c' {
  c( d e\( d~ d1) g'4 a b f | e\) d2\laissezVibrer r4 | r c2.\repeatTie
}

% this example shows how displayed control-points affect layout, but
% only in case of ties - there are no problems with Slurs and PhrasingSlurs!
%
%
% Workaround:
% insert
%     \override Tie #'vertical-skylines = #'()
% as shown below.

%%{
\relative c' {
  \override Slur #'stencil = #(display-control-points 0.2 '(1.0 0.0 1.0) 0.3 2 red)
  \override PhrasingSlur #'stencil = #(display-control-points 0.2 red 0.3 2 (x11-color 'SlateBlue2))
  \override Tie #'stencil = #(display-control-points 0.2 red 0.3 2 red)
  % bug-workaround for 2.17.x:
  \override Tie #'vertical-skylines = #'()
  %% Modifying 'layer may be of some use.
  %% TODO: If wanted, affect 'layer of all dynamics, markups, etc
  %% UL: I don't think that's necessary.
  \override Tie #'layer = #-10
  \override DynamicText #'layer = #10

  c1~\ppp c a''~^\ppp a
  f,\(\ppp d\) g'\(^\ppp e\)
  f,(\ppp d) g'(^\ppp e)
}
%}
