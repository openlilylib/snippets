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
% modified code from stencil.scm
%
% make-path-stencil modified in place to use specified line cap and
% line join styles, not round as per the hard coded default.
%=========================================================

#(define-public (make-path-stencil-with-line-styles path thickness x-scale y-scale fill line-cap-style line-join-style)
   "Make a stencil based on the path described by the list @var{path},
with thickness @var{thickness}, and scaled by @var{x-scale} in the X
direction and @var{y-scale} in the Y direction.  @var{fill} is a boolean
argument that specifies if the path should be filled.  Valid path
commands are: moveto rmoveto lineto rlineto curveto rcurveto closepath,
and their standard SVG single letter equivalents: M m L l C c Z z."

   ;;=========================================================
   ;; helpers from stencil.scm
   ;; placed here to prevent any top level future clashes.
   ;;=========================================================

   (define (line-part-min-max x1 x2)
     (list (min x1 x2) (max x1 x2)))

   (define (line-min-max x1 y1 x2 y2)
     (map (lambda (x)
            (apply line-part-min-max x))
       `((,x1 ,x2) (,y1 ,y2))))

   (define (path-min-max origin pointlist)

     ((lambda (x)
        (list
         (reduce min +inf.0 (map caar x))
         (reduce max -inf.0 (map cadar x))
         (reduce min +inf.0 (map caadr x))
         (reduce max -inf.0 (map cadadr x))))
      (map (lambda (x)
             (if (= (length x) 8)
                 (apply bezier-min-max x)
                 (apply line-min-max x)))
        (map (lambda (x y)
               (append (list (cadr (reverse x)) (car (reverse x))) y))
          (append (list origin)
            (reverse (cdr (reverse pointlist)))) pointlist))))

   (define (convert-path path origin previous-point)
     "Recursive function to standardize command names and
convert any relative path expressions (in @var{path}) to absolute
values.  Returns a list of lists.  @var{origin} is a pair of x and y
coordinates for the origin point of the path (used for closepath and
reset by moveto commands).  @var{previous-point} is a pair of x and y
coordinates for the previous point in the path."
     (if (pair? path)
         (let*
          ((head-raw (car path))
           (rest (cdr path))
           (head (cond
                  ((memq head-raw '(rmoveto M m)) 'moveto)
                  ((memq head-raw '(rlineto L l)) 'lineto)
                  ((memq head-raw '(rcurveto C c)) 'curveto)
                  ((memq head-raw '(Z z)) 'closepath)
                  (else head-raw)))
           (arity (cond
                   ((memq head '(lineto moveto)) 2)
                   ((eq? head 'curveto) 6)
                   (else 0)))
           (coordinates-raw (take rest arity))
           (is-absolute (if (memq head-raw
                              '(rmoveto m rlineto l rcurveto c)) #f #t))
           (coordinates (if is-absolute
                            coordinates-raw
                            ;; convert relative coordinates to absolute by
                            ;; adding them to previous point values
                            (map (lambda (c n)
                                   (if (even? n)
                                       (+ c (car previous-point))
                                       (+ c (cdr previous-point))))
                              coordinates-raw
                              (iota arity))))
           (new-point (if (eq? head 'closepath)
                          origin
                          (cons
                           (list-ref coordinates (- arity 2))
                           (list-ref coordinates (- arity 1)))))
           (new-origin (if (eq? head 'moveto)
                           new-point
                           origin)))
          (cons (cons head coordinates)
            (convert-path (drop rest arity) new-origin new-point)))
         '()))

   (let* ((path-absolute (convert-path path (cons 0 0) (cons 0 0)))
          ;; scale coordinates
          (path-scaled (if (and (= 1 x-scale) (= 1 y-scale))
                           path-absolute
                           (map (lambda (path-unit)
                                  (map (lambda (c n)
                                         (cond
                                          ((= 0 n) c)
                                          ((odd? n) (* c x-scale))
                                          (else (* c y-scale))))
                                    path-unit
                                    (iota (length path-unit))))
                             path-absolute)))
          ;; a path must begin with a 'moveto'
          (path-final (if (eq? 'moveto (car (car path-scaled)))
                          path-scaled
                          (append (list (list 'moveto 0 0)) path-scaled)))
          ;; remove all commands in order to calculate bounds
          (path-headless (map cdr (delete (list 'closepath) path-final)))
          (bound-list (path-min-max
                       (car path-headless)
                       (cdr path-headless))))
     (ly:make-stencil
      `(path ,thickness
         `(,@',(concatenate path-final))
         ,line-cap-style
         ,line-join-style
         ,(if fill #t #f))
      (coord-translate
       ((if (< x-scale 0) reverse-interval identity)
        (cons
         (list-ref bound-list 0)
         (list-ref bound-list 1)))
       `(,(/ thickness -2) . ,(/ thickness 2)))
      (coord-translate
       ((if (< y-scale 0) reverse-interval identity)
        (cons
         (list-ref bound-list 2)
         (list-ref bound-list 3)))
       `(,(/ thickness -2) . ,(/ thickness 2))))))

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
                 (make-path-stencil-with-line-styles
                  (list
                   'moveto start-x start-y
                   'lineto end-x end-y)
                  thickness
                  1
                  1
                  #t
                  ''butt
                  ''miter
                  ))))))
   #})

slashStemOff =
{
  \revert Stem.stencil
}

