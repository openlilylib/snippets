\version "2.19.45"

\header {
  snippet-title = "Slash Beams"
  snippet-author = "Andrew Bernard"
  snippet-author-email = "andrew.bernard@gmail.com"
  snippet-source = ""
  snippet-description = \markup {
    Add slash to a beamed group, on the right or on the left. This code provides
    the "\slashBeam" function.
  }
  tags = "beams, slash, grace, acciaccatura"
  status = "ready"
}

slashBeam =
#(define-music-function (side slash-angle beam-fraction outer-proportion)
   ((symbol? 'left) number? number? number?)
   "Make a slash on a beamed group.
   side             - 'left or 'right. If omitted, defaults to 'left.
   slash-angle      - angle of slash with respect to x axis.
   beam-fraction    - intersection point of slash with beam, as a fraction of beam length,
                      measured from the left hand side.
   outer-proportion - length of the outside projections as a proportion of the length of the
                      inner segment between beam and stem.
   "
   ;; helpers
   (define (square x)
     (* x x))

   (define (euclidean-len start-x start-y end-x end-y)
     (sqrt (+ (square (- end-x start-x)) (square (- end-y start-y)))))

   (let ((input-loc (*location*)))

     ;; main code
     #{
       \once \override Beam.stencil =
       #(lambda (grob)
          (let* (
                  ;; get beam stencil
                  (beam-stil (ly:beam::print grob))

                  ;; which side?
                  (on-left
                   (case side
                     ((left) #t)
                     ((right) #f)
                     (else
                      (ly:input-warning input-loc
                        "Unknown setting for \\slashBeam side argument: \"~a\". Default to 'left." side)
                      #t)))
                  ;                 (eq? side 'left))

                  ;; get beam direction, up or down. can be found by examining stem direction.
                  (stems (ly:grob-object grob 'stems))
                  ;; on the left, use the first stem. on the right, use last stem. this allows the function
                  ;; to work with kneed beams as well.
                  (stem (if on-left
                            (ly:grob-array-ref stems 0)
                            (ly:grob-array-ref stems (- (ly:grob-array-length stems) 1))))
                  (stem-dir (ly:grob-property stem 'direction #f))
                  ;; translate to boolean regarding the beam purely for logical elegance,
                  ;; since we are oriented on working with the beam.
                  (beam-up (= 1 stem-dir))

                  ;; get beam dimensional properties
                  (beam-X-pos (ly:grob-property grob 'X-positions))
                  (beam-start-x (car beam-X-pos))
                  (beam-end-x (cdr beam-X-pos))
                  (beam-Y-pos (ly:grob-property grob 'positions))
                  (beam-start-y (car beam-Y-pos))
                  (beam-end-y (cdr beam-Y-pos))
                  (beam-dy (- beam-end-y beam-start-y))
                  (beam-dx (- beam-end-x beam-start-x))
                  (beam-gradient (/ beam-dy beam-dx))
                  (beam-length (sqrt (+ (* beam-dy beam-dy) (* beam-dx beam-dx)))) ;; Euclidean length
                  (beam-angle (atan beam-gradient))
                  (beam-slope-up (>= beam-angle 0.0))
                  (beam-fraction-a (* beam-length beam-fraction)) ;; from start to intersection point
                  (beam-fraction-b (* beam-length (- 1 beam-fraction))) ;; from intersection point to end
                  (slash-angle (degrees->radians slash-angle))
                  (thickness (ly:staff-symbol-line-thickness grob))

                  ;; geometrical computation of position for line endpoints between beam and stem.
                  ;; we draw the slash from the beam to the stem.
                  (start-x (if on-left
                               ;; on left
                               (if beam-up
                                   ;; beam up
                                   (if beam-slope-up
                                       ;; beam up, slope up
                                       (* beam-fraction-a (cos beam-angle))
                                       ;; beam up, slope down
                                       (* beam-fraction-a (cos beam-angle)))
                                   ;; beam down
                                   (if beam-slope-up
                                       ;; beam down, slope up
                                       (* beam-fraction-a (cos beam-angle))
                                       ;; beam down, slope down
                                       (* beam-fraction-a (cos beam-angle))))
                               ;; on right
                               (if beam-up
                                   ;; beam up
                                   (if beam-slope-up
                                       ;; beam up, slope up
                                       (* beam-fraction-a (cos beam-angle))
                                       ;; beam up, slope down
                                       (* beam-fraction-a (cos beam-angle))
                                       )
                                   ;; beam down
                                   (if beam-slope-up
                                       ;; beam down, slope up
                                       (* beam-fraction-a (cos beam-angle))
                                       ;; beam down, slope down
                                       (* beam-fraction-a (cos beam-angle))))))

                  (start-y (if on-left
                               ;; on left
                               (if beam-up
                                   ;; beam up
                                   (if beam-slope-up
                                       ;; beam up, slope up
                                       (+ beam-start-y (* beam-fraction-a (sin beam-angle)))
                                       ;; beam up, slope down
                                       (- beam-start-y (* beam-fraction-a (- (sin beam-angle)))))
                                   ;; beam down
                                   (if beam-slope-up
                                       ;; beam down, slope up
                                       (+ beam-start-y (* beam-fraction-a (sin beam-angle)))
                                       ;; beam up, slope down
                                       (- beam-start-y (* beam-fraction-a (- (sin beam-angle))))))
                               ;; on right
                               (if beam-up
                                   (if beam-slope-up
                                       ;; beam up, slope up
                                       (- beam-end-y (* beam-fraction-b (sin beam-angle)))
                                       ;; beam up, slope down
                                       (+ beam-end-y (* beam-fraction-b (- (sin beam-angle)))))
                                   (if beam-slope-up
                                       ;; beam down, slope up
                                       (- beam-end-y (* beam-fraction-b (sin beam-angle)))
                                       ;; beam down, slope down
                                       (+ beam-end-y (* beam-fraction-b (- (sin beam-angle))))))))

                  (end-x (if on-left
                             ;; on left
                             (if beam-up
                                 ;; beam up
                                 (if beam-slope-up
                                     ;; beam up, slope up
                                     0
                                     ;; beam up, slope down
                                     0)
                                 ;; beam down
                                 (if beam-slope-up
                                     ;; beam down, slope up
                                     0
                                     ;; beam down, slope down
                                     0))
                             ;; on right
                             (if beam-up
                                 ;; beam up
                                 (if beam-slope-up
                                     ;; beam up, slope up
                                     (* beam-length (cos beam-angle))
                                     ;; beam down, slope down
                                     (* beam-length (cos beam-angle)))
                                 ;; beam down
                                 (if beam-slope-up
                                     ;; beam down, slope up
                                     (* beam-length (cos beam-angle))
                                     ;; beam down, slope down
                                     (* beam-length (cos beam-angle))))))

                  (end-y (if on-left
                             ;; on left
                             (if beam-up
                                 ;; beam up
                                 (if beam-slope-up
                                     ;; beam up, slope up
                                     (- beam-start-y (* beam-fraction-a (- (* (cos beam-angle) (tan slash-angle)) (sin beam-angle))))
                                     ;; beam up, slope down
                                     (- beam-start-y (* beam-fraction-a (+ (* (cos beam-angle) (tan slash-angle)) (sin beam-angle)))))
                                 ;; beam down
                                 (if beam-slope-up
                                     ;; beam down, slope up
                                     (+ beam-start-y (* beam-fraction-a (+ (* (cos beam-angle) (tan slash-angle)) (sin beam-angle))))
                                     ;; beam down, slope down
                                     (+ beam-start-y (* beam-fraction-a (- (* (cos beam-angle) (tan slash-angle)) (- (sin beam-angle)))))))
                             ;; on right
                             (if beam-up
                                 ;; beam up
                                 (if beam-slope-up
                                     ;; beam up, slope up
                                     (- beam-end-y (* beam-fraction-b (+ (* (cos beam-angle) (tan slash-angle)) (sin beam-angle))))
                                     ;; beam up, slope down
                                     (- beam-end-y (* beam-fraction-b (- (* (cos beam-angle) (tan slash-angle)) (- (sin beam-angle))))))
                                 ;; beam down
                                 (if beam-slope-up
                                     ;; beam down, slope up
                                     (+ beam-end-y (* beam-fraction-b (- (* (cos beam-angle) (tan slash-angle)) (sin beam-angle))))
                                     ;; beam down, slope down

                                     (+ beam-end-y (* beam-fraction-b (+ (* (cos beam-angle) (tan slash-angle)) (sin beam-angle))))))))

                  ;; get slash inner length (between beam and stem)
                  (slash-inner-length (euclidean-len start-x start-y end-x end-y))

                  ;; add the outer slash projections, past the beam and stem
                  (projection-dx (* (cos slash-angle) (* slash-inner-length outer-proportion)))
                  (projection-dy (* (sin slash-angle) (* slash-inner-length outer-proportion)))

                  (start-x (if on-left
                               (if beam-up
                                   (+ start-x projection-dx)
                                   (+ start-x projection-dx))
                               (if beam-up
                                   (- start-x projection-dx)
                                   (- start-x projection-dx))))

                  (start-y (if on-left
                               (if beam-up
                                   (+ start-y projection-dy)
                                   (- start-y projection-dy))
                               (if beam-up
                                   (+ start-y projection-dy)
                                   (- start-y projection-dy))))

                  (end-x (if on-left
                             (if beam-up
                                 (- end-x projection-dx)
                                 (- end-x projection-dx))
                             (if beam-up
                                 (+ end-x projection-dx)
                                 (+ end-x projection-dx))))

                  (end-y (if on-left
                             (if beam-up
                                 (- end-y projection-dy)
                                 (+ end-y projection-dy))
                             (if beam-up
                                 (- end-y projection-dy)
                                 (+ end-y projection-dy))))

                  ;; create slash
                  (slash (make-line-stencil thickness start-x start-y end-x end-y)))

            ;; return modified beam stencil
            (ly:stencil-add beam-stil slash)))
     #}))
