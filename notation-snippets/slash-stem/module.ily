\version "2.19.49"

\header {
  snippet-title = "Slash Stems"
  snippet-author = "Andrew Bernard"
  snippet-author-email = "andrew.bernard@gmail.com"
  snippet-source = ""
  snippet-description = \markup {
    Add slash to a stem. This code provides the "\slashStem" function.
  }
  tags = "stem, slash"
  status = "ready"
}

%=========================================================
% music functions
%=========================================================

slashStem =
#(define-music-function (length fraction thickness angle)
   (number? number? number? number?)
   "Create a slash on a stem.
  length    - length of slash, in staff space units.
  fraction  - proportional point on the stem to place the slash, from 0 to 1.0.
  thickness - line thickness of the slash.
  angle     - angle of slash with respect to horizontal axis, in degrees.
  "
   #{
     \override Stem.stencil =
     #(lambda (grob)
        (let* (
                (x-parent (ly:grob-parent grob X))
                (is-rest? (ly:grob? (ly:grob-object x-parent 'rest))))
          (if is-rest?
              empty-stencil
              (let* (
                      ;; must get length from stencil, not from grob
                      (stil (ly:stem::print grob))
                      (stem-Y-ext (ly:stencil-extent stil Y))
                      (stem-length (interval-length stem-Y-ext))
                      (stem-dir (ly:grob-property grob 'direction))
                      (angle (degrees->radians angle))
                      (start-x (- (/ length 2)))
                      (end-x (/ length 2))
                      ;; where to cut the stem with slash
                      (slash-point (* stem-length fraction))
                      ;; compute offset based on fractional position
                      (start-y-offset (if (= 1 stem-dir)
                                          (- (cdr stem-Y-ext) slash-point)
                                          (+ (car stem-Y-ext) slash-point)))
                      (start-y (- start-y-offset (/ (* length (sin angle))2)))
                      (end-y (+ start-y-offset (/ (* length (sin angle)) 2)))
                      (start-x (- (/ (* length (cos angle)) 2)))
                      (end-x (/ (* length (cos angle)) 2)))

                (ly:stencil-add
                 stil
                 (make-path-stencil
                  (list
                   'moveto start-x start-y
                   'lineto end-x end-y)
                  thickness 1 1 #t
                  #:line-cap-style 'butt #:line-join-style 'miter))))))
   #})

slashStemOff =
{
  \revert Stem.stencil
}

