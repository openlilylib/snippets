\version "2.16.2"

% use this to crop output page size to match the snippet
#(ly:set-option 'preview #t)

\header {
  snippet-title = "Displaying control points of slurs and ties"
  snippet-author = "?"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "slur, tie, bezier curve, control point, preview mode"
  % is this snippet ready?  See meta/status-values.md
  status = ""
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-cross-stencil coords)
   (ly:stencil-add
    (make-line-stencil 0.1 (- (car coords) 0.2) (- (cdr coords) 0.2)
      (+ (car coords) 0.2) (+ (cdr coords) 0.2))
    (make-line-stencil 0.1 (- (car coords) 0.2) (+ (cdr coords) 0.2)
      (+ (car coords) 0.2) (- (cdr coords) 0.2))))

#(define (display-control-points)
   (lambda (grob)
     (let* ((grob-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
            (name (grob-name grob))
            (stil (cond ((or (eq? name 'Slur)(eq? name 'PhrasingSlur))(ly:slur::print grob))
                    ((eq? name 'Tie)(ly:tie::print grob))))
            (cps (ly:grob-property grob 'control-points)))

       (ly:stencil-add stil
         (ly:stencil-in-color
          (ly:stencil-add
           (make-cross-stencil (second cps))
           (make-cross-stencil (third cps))
           )
          1 0 0) ; color is hard-coded here (R G B).
         ; modify to change the color of the crosses

         (ly:stencil-in-color
          (ly:stencil-add
           (make-line-stencil 0.05
             (car (first cps)) (cdr (first cps))
             (car (second cps))  (cdr (second cps)))
           ;(make-line-stencil 0.05 (car (second cps)) (cdr (second cps)) (car (third cps))  (cdr (third cps)))
           (make-line-stencil 0.05
             (car (third cps)) (cdr (third cps))
             (car (fourth cps))  (cdr (fourth cps)))
           )
          1 0 0) ; color is hard-coded here (R G B).
         ; modify to change the color of the lines
         empty-stencil)
       )
     ))

% The following functions work in the current context
% So you can place it in the music input and modify the
% following music.

% Switch on the display of control-points for the current Voice
displayControlPoints = {
  \override Slur #'stencil = #(display-control-points)
  \override PhrasingSlur #'stencil = #(display-control-points)
  \override Tie #'stencil = #(display-control-points)
}

% Switch on the display of the control-points globally
% Place in a \layout block
debugCurvesOn = \layout {
  \context {
    \Score
    \override Slur #'stencil = #(display-control-points)
    \override PhrasingSlur #'stencil = #(display-control-points)
    \override Tie #'stencil = #(display-control-points)
  }
}


\layout {
  \debugCurvesOn
}

\relative c' {
  %\displayControlPoints
  c( d e\( d c1) g'4 a b f | e\)
}

{
  c'1~ \ppp c'
}