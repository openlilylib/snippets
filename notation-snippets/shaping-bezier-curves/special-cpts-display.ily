\version "2.17.22"

\header {
  snippet-title = "special displayControlPoints"
  snippet-description = \markup {
    This modified version of displayControlPoints shows a blue line
    connecting first and last control-points.
  }
  status = "hack"
}

% Define appearance
#(cond ((not (defined? 'debug-control-points-line-thickness))
        (define debug-control-points-line-thickness 0.05)))
#(cond ((not (defined? 'debug-control-points-cross-size))
        (define debug-control-points-cross-size 0.7)))
#(cond ((not (defined? 'debug-control-points-color))
        (define debug-control-points-color red)))

#(define (make-cross-stencil coords cross-thickness arm-offset)
   ;; coords are the coordinates of the center of the cross
   (ly:stencil-add
    (make-line-stencil
     debug-control-points-line-thickness
     (- (car coords) arm-offset)
     (- (cdr coords) arm-offset)
     (+ (car coords) arm-offset)
     (+ (cdr coords) arm-offset))
    (make-line-stencil
     debug-control-points-line-thickness
     (- (car coords) arm-offset)
     (+ (cdr coords) arm-offset)
     (+ (car coords) arm-offset)
     (- (cdr coords) arm-offset))))

#(define (special-control-points )
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
              ;(make-cross-stencil (first ctrpts)
              ;debug-control-points-line-thickness
              ;(/ debug-control-points-cross-size 2.8284))
              (make-cross-stencil (second ctrpts)
                debug-control-points-line-thickness
                (/ debug-control-points-cross-size 2.8284))
              (make-cross-stencil (third ctrpts)
                debug-control-points-line-thickness
                (/ debug-control-points-cross-size 2.8284))
              ;(make-cross-stencil (fourth ctrpts)
              ;debug-control-points-line-thickness
              ;(/ debug-control-points-cross-size 2.8284))
              ))
            (line-stencils
             (ly:stencil-add
              (make-line-stencil debug-control-points-line-thickness
                (car (first ctrpts)) (cdr (first ctrpts))
                (car (second ctrpts))  (cdr (second ctrpts)))
              ;; If you want a line from second to third control-point uncomment
              ;; the following expression.
              (make-line-stencil debug-control-points-line-thickness
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
       ;; as shown in the main function below.
       (ly:stencil-add stil
         ;; add crosses:
         (ly:make-stencil
          (ly:stencil-expr (stencil-with-color
                            cross-stencils
                            debug-control-points-color))
          empty-interval
          empty-interval)

         ;; add lines:
         (ly:make-stencil
          (ly:stencil-expr (stencil-with-color
                            line-stencils
                            debug-control-points-color))
          empty-interval
          empty-interval)
         (ly:make-stencil
          (ly:stencil-expr (stencil-with-color
                            (ly:stencil-add
                             (make-line-stencil 0.1
                               (car (first ctrpts)) (cdr (first ctrpts))
                               (car (fourth ctrpts))  (cdr (fourth ctrpts))))
                            blue))
          empty-interval
          empty-interval)
         empty-stencil))))
