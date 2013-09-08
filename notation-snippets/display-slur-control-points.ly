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
}

\layout {
  \displayControlPoints
}

\relative c' {
  c( d e\( d c1) g'4 a b f | e\)
}

{
  c'1~ \ppp c'
}