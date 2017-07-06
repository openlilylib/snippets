\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Guitar string bending notation"
  snippet-author = "Marc Hohl"
  snippet-source = "http://code.google.com/p/lilypond/issues/detail?id=1196"
  snippet-description = \markup {
    This snippet allows to typeset bend symbols -
    typically used on guitar - on Staff and TabStaff.
    While issue 1196 aims to create a specific engraver
    for bends, this snippet leverages the slur engraver.
  }
  % add comma-separated tags to make searching more effective:
  tags = "guitar, bend, string bending"
  % is this snippet ready?  See meta/status-values.md
  status = "buggy"
}

% TODO:
% - draw dashed line for \holdBend
% - enable consecutive bend ups
% - simplify \preBend and \holdBend usage
% - ...

#(display "\nbend.ly ─ 2011-03-11 (revised: 2013-07-16)\n\n")

%%% sizes and values (to be changed/adapted):

#(define bend-line-thickness 0.1)

#(define bend-arrow-curvature-factor 0.35)

#(define y-distance-from-tabstaff-to-arrow-tip 2.75)

#(define consecutive-bends-arrow-height 2.75)

#(define bend-arrowhead-height 1.25)

#(define bend-arrowhead-width 0.8)

#(define y-distance-from-staffline-to-arrow 0.35)

%%% internal commands
#(define (quarterdiff->string quarterdiff)
   (let ((wholesteps (floor (/ quarterdiff 4))))

     (string-append (case wholesteps
                      ((0) "")
                      (else (number->string wholesteps)))
       (case (modulo quarterdiff 4)
         ((1) "¼")
         ((2) "½")
         ((3) "¾")
         (else "")))))

%%% markup commands

#(define-markup-command (pointedSlur layout props thickness bx by mx my ex ey)
   (number? number? number? number? number? number? number?)
   (interpret-markup layout props
     (markup #:postscript
       (ly:format "~f setlinewidth
                        ~f ~f moveto
                        ~f ~f lineto
                        ~f ~f lineto stroke" thickness bx by mx my ex ey))))

#(define-markup-command (drawBendArrow layout props
                          thickness begin-x middle-x end-x begin-y end-y arrow-lx arrow-rx arrow-y outstring)
   (number? number? number? number? number? number? number? number? number? string?)
   (interpret-markup layout props
     (markup #:postscript
       (ly:format "~f setlinewidth
                        ~f ~f moveto
                        ~f ~f lineto
                        ~f ~f ~f ~f ~f ~f curveto
                        stroke
                        ~f ~f moveto
                        ~f ~f lineto
                        ~f ~f lineto
                        closepath fill"
thickness
begin-x begin-y
middle-x begin-y
middle-x begin-y end-x begin-y end-x arrow-y
arrow-lx arrow-y
end-x end-y
arrow-rx arrow-y)
       #:hspace 0
       #:translate (cons (- end-x 1.2) (+ end-y 0.5))
       #:fontsize -2
       #:bold
       ;; changed:
       ;#:center-column (outstring)
       outstring
       )))
#(define-markup-command (drawHoldBendWithArrow layout props
                          thickness begin-x begin-y end-x end-y arrow-lx arrow-rx arrow-y outstring)
   (number? number? number? number? number? number? number? number? string?)
   (interpret-markup layout props
     (markup #:postscript
       (ly:format "~f setlinewidth
                        ~f ~f moveto
                        ~f ~f lineto
                        stroke
                        ~f ~f moveto
                        ~f ~f lineto
                        ~f ~f lineto
                        closepath fill
                        ~f ~f moveto
                        ~f ~f lineto
                        stroke"
thickness
begin-x begin-y
begin-x arrow-y
arrow-lx arrow-y
begin-x end-y
arrow-rx arrow-y
begin-x end-y
end-x end-y)
       #:hspace 0
       #:translate (cons (- begin-x 1.2) (+ end-y 0.5))
       #:fontsize -2
       ;; Why does it work here??
       #:bold #:center-column (outstring))))

#(define-markup-command (drawHoldBendArrowOnly layout props
                          thickness begin-x begin-y end-x end-y arrow-lx arrow-rx arrow-y outstring)
   (number? number? number? number? number? number? number? number? string?)
   (interpret-markup layout props
     (markup #:postscript
       (ly:format "~f setlinewidth
                        ~f ~f moveto
                        ~f ~f lineto
                        stroke
                        ~f ~f moveto
                        ~f ~f lineto
                        ~f ~f lineto
                        closepath fill"
thickness
begin-x begin-y
begin-x arrow-y
arrow-lx arrow-y
begin-x end-y
arrow-rx arrow-y)
       #:hspace 0
       #:translate (cons (- begin-x 1.2) (+ end-y 0.5))
       #:fontsize -2
       ;; Why does it work here??
       #:bold #:center-column (outstring))))

%% The markup-command 'draw-dashed-line' was implemented with version 2.17.x
%% TODO: use 'draw-dashed-line' instead. See also 'tie::draw-hold-bend' below.
#(define-markup-command (drawDashedLine layout props
                          thickness begin-x end-x line-y)
   (number? number? number? number?)
   ;; TODO: draws a full line instead of a dashed line
   (interpret-markup layout props
     (markup #:postscript
       (ly:format "~f setlinewidth
                        ~f ~f moveto
                        ~f ~f lineto
                        stroke"
thickness begin-x line-y end-x line-y))))

%%% callbacks

#(define (slur::draw-pointed-slur grob)
   (let* ((control-points (ly:grob-property grob 'control-points))
          (direction (ly:grob-property grob 'direction))
          (first-point (car control-points))
          (second-point (cadr control-points))
          (third-point (caddr control-points))
          (forth-point (cadddr control-points))
          (first-x (+ (car first-point) 0.125)) ;; due to David's proposals
          (first-y (cdr first-point))
          (second-x (car second-point))
          (second-y (cdr second-point))
          (third-x (car third-point))
          (third-y (cdr third-point))
          (forth-x (- (car forth-point) 0.125))
          (forth-y (cdr forth-point))

          (middle-x (/ (+ third-x second-x) 2))
          (middle-y (/ (+ third-y second-y) 2)))

     (grob-interpret-markup grob
       (make-pointedSlur-markup bend-line-thickness
         first-x first-y middle-x middle-y forth-x forth-y))))

#(define (slur::draw-bend-arrow grob)
   (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
          (line-count (ly:grob-property staff-symbol 'line-count))
          (staff-space (ly:grob-property staff-symbol 'staff-space))
          (left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (left-tab-note-head (ly:grob-property left-bound 'cause))
          (right-tab-note-head (ly:grob-property right-bound 'cause))
          (control-points (ly:grob-property grob 'control-points))
          (left-point (car control-points))
          ;;changed: cadddr changed to last
          (right-point (last control-points))
          (left-pitch  (ly:event-property (event-cause left-bound) 'pitch))
          (right-pitch (ly:event-property (event-cause right-bound) 'pitch))
          (quarterdiff (- (ly:pitch-quartertones right-pitch)
                         (ly:pitch-quartertones left-pitch)))
          (begin-x (car left-point))
          (begin-y (+ (* (/ (ly:grob-property left-tab-note-head 'staff-position) 2)
                        staff-space)
                     y-distance-from-staffline-to-arrow))
          ;; cdr left-point doesn't work, because invisible stems are included
          (end-x (car right-point))
          (end-y (+ (* (/ (- line-count 1) 2) staff-space) y-distance-from-tabstaff-to-arrow-tip))
          (arrow-lx (- end-x (/ bend-arrowhead-width 2)))
          (arrow-rx (+ end-x (/ bend-arrowhead-width 2)))
          (arrow-y (- end-y bend-arrowhead-height))
          (middle-x (+ begin-x (* bend-arrow-curvature-factor (- end-x begin-x))))
          (bend-amount (quarterdiff->string quarterdiff)))

     (if (< quarterdiff 0)
         ;; bend down
         (let* ((y-offset (cdr (ly:grob-extent left-tab-note-head left-tab-note-head Y)))
                (temp begin-y))
           (set! begin-y end-y) ;; swap begin-y/end-y
           (set! end-y (+ temp y-offset))
           (set! arrow-y (+ end-y bend-arrowhead-height))
           (set! bend-amount "")
           (ly:grob-set-property! right-tab-note-head 'display-cautionary #t)
           (ly:grob-set-property! right-tab-note-head 'stencil tab-note-head::print))
         ;; bend up
         (let* ((x-offset (/ (cdr (ly:grob-extent left-tab-note-head left-tab-note-head X))
                            2)))

           (set! begin-x (+ begin-x x-offset))
           (ly:grob-set-property! right-tab-note-head 'transparent #t)))

     ;; draw resulting bend arrow
     (grob-interpret-markup grob
       (make-drawBendArrow-markup
        bend-line-thickness
        begin-x middle-x end-x begin-y end-y
        arrow-lx arrow-rx arrow-y
        bend-amount))))


#(define (slur::draw-shifted-bend-arrow grob)
   (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
          (line-count (ly:grob-property staff-symbol 'line-count))
          (staff-space (ly:grob-property staff-symbol 'staff-space))
          (left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (left-tab-note-head (ly:grob-property left-bound 'cause))
          (right-tab-note-head (ly:grob-property right-bound 'cause))
          (control-points (ly:grob-property grob 'control-points))
          (left-point (car control-points))
          (right-point (cadddr control-points))
          (left-pitch  (ly:event-property (event-cause left-bound) 'pitch))
          (right-pitch (ly:event-property (event-cause right-bound) 'pitch))
          (quarterdiff (- (ly:pitch-quartertones right-pitch)
                         (ly:pitch-quartertones left-pitch)))
          (begin-x (car left-point))
          (begin-y (+ (* (/ (ly:grob-property left-tab-note-head 'staff-position) 2)
                        staff-space)
                     y-distance-from-tabstaff-to-arrow-tip))
          ;; cdr left-point doesn't work, because invisible stems are included
          (end-x (car right-point))
          (end-y (+ (* (/ (- line-count 1) 2) staff-space) y-distance-from-tabstaff-to-arrow-tip consecutive-bends-arrow-height))
          (arrow-lx (- end-x (/ bend-arrowhead-width 2)))
          (arrow-rx (+ end-x (/ bend-arrowhead-width 2)))
          (arrow-y (- end-y bend-arrowhead-height))
          (middle-x (+ begin-x (* bend-arrow-curvature-factor (- end-x begin-x))))
          (bend-amount (quarterdiff->string quarterdiff)))
     (if (< quarterdiff 0)
         ;; bend down
         (let* ((y-offset (cdr (ly:grob-extent left-tab-note-head left-tab-note-head Y)))
                (temp begin-y))

           (set! begin-y end-y) ;; swap begin-y/end-y
           (set! end-y (+ temp y-offset))
           (set! arrow-y (+ end-y bend-arrowhead-height))
           (set! bend-amount "")
           (ly:grob-set-property! right-tab-note-head 'stencil
             (lambda (grob) (parenthesize-tab-note-head grob))))
         ;; bend up
         (ly:grob-set-property! right-tab-note-head 'transparent #t))
     ;; draw resulting bend arrow
     (grob-interpret-markup grob
       (make-drawBendArrow-markup
        bend-line-thickness
        begin-x middle-x end-x begin-y end-y
        arrow-lx arrow-rx arrow-y
        bend-amount))))

#(define (slur::draw-pre-bend-hold grob)
   (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
          (line-count (ly:grob-property staff-symbol 'line-count))
          (staff-space (ly:grob-property staff-symbol 'staff-space))
          (left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (left-tab-note-head (ly:grob-property left-bound 'cause))
          (right-tab-note-head (ly:grob-property right-bound 'cause))
          (control-points (ly:grob-property grob 'control-points))
          (left-point (car control-points))
          (right-point (cadddr control-points))
          (left-pitch  (ly:event-property (event-cause left-bound) 'pitch))
          (right-pitch (ly:event-property (event-cause right-bound) 'pitch))
          (quarterdiff (- (ly:pitch-quartertones right-pitch)
                         (ly:pitch-quartertones left-pitch)))
          (begin-x (car left-point))
          (y-offset (cdr (ly:grob-extent left-tab-note-head left-tab-note-head Y)))
          (begin-y (+ (* (/ (ly:grob-property left-tab-note-head 'staff-position)
                           2)
                        staff-space)
                     y-offset))
          ;; cdr left-point doesn't work, because invisible stems are included
          (end-x (car right-point))
          (end-y (+ (* (/ (- line-count 1) 2) staff-space) y-distance-from-tabstaff-to-arrow-tip))
          (arrow-lx (- begin-x (/ bend-arrowhead-width 2)))
          (arrow-rx (+ begin-x (/ bend-arrowhead-width 2)))
          (arrow-y (- end-y bend-arrowhead-height))
          (bend-amount (quarterdiff->string quarterdiff)))

     (ly:grob-set-property! right-tab-note-head 'transparent #t)
     ;; draw resulting bend arrow
     (grob-interpret-markup grob
       (make-drawHoldBendWithArrow-markup
        bend-line-thickness
        begin-x begin-y
        end-x end-y
        arrow-lx arrow-rx arrow-y
        bend-amount))))

#(define (slur::draw-pre-bend-only grob)
   (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
          (line-count (ly:grob-property staff-symbol 'line-count))
          (staff-space (ly:grob-property staff-symbol 'staff-space))
          (left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (left-tab-note-head (ly:grob-property left-bound 'cause))
          (right-tab-note-head (ly:grob-property right-bound 'cause))
          (control-points (ly:grob-property grob 'control-points))
          (left-point (car control-points))
          (right-point (cadddr control-points))
          (left-pitch  (ly:event-property (event-cause left-bound) 'pitch))
          (right-pitch (ly:event-property (event-cause right-bound) 'pitch))
          (quarterdiff (- (ly:pitch-quartertones right-pitch)
                         (ly:pitch-quartertones left-pitch)))
          (begin-x (car left-point))
          (y-offset (cdr (ly:grob-extent left-tab-note-head left-tab-note-head Y)))
          (begin-y (+ (* (/ (ly:grob-property left-tab-note-head 'staff-position)
                           2)
                        staff-space)
                     y-offset))
          ;; cdr left-point doesn't work, because invisible stems are included
          (end-x (car right-point))
          (end-y (+ (* (/ (- line-count 1) 2) staff-space) y-distance-from-tabstaff-to-arrow-tip))
          (arrow-lx (- begin-x (/ bend-arrowhead-width 2)))
          (arrow-rx (+ begin-x (/ bend-arrowhead-width 2)))
          (arrow-y (- end-y bend-arrowhead-height))
          (bend-amount (quarterdiff->string quarterdiff)))

     (ly:grob-set-property! right-tab-note-head 'transparent #t)
     ;; draw resulting bend arrow
     (grob-interpret-markup grob
       (make-drawHoldBendArrowOnly-markup
        bend-line-thickness
        begin-x begin-y
        end-x end-y
        arrow-lx arrow-rx arrow-y
        bend-amount))))

#(define (tie::draw-hold-bend grob)
   (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
          (line-count (ly:grob-property staff-symbol 'line-count))
          (staff-space (ly:grob-property staff-symbol 'staff-space))
          (left-tab-note-head (ly:spanner-bound grob LEFT))
          (right-tab-note-head (ly:spanner-bound grob RIGHT))
          (control-points (ly:grob-property grob 'control-points))
          (left-point (car control-points))
          (right-point (cadddr control-points))
          (begin-x (car left-point))
          (end-x (car right-point))
          (line-y (+ (* (/ (- line-count 1) 2) staff-space) y-distance-from-tabstaff-to-arrow-tip)))

     (ly:grob-set-property! right-tab-note-head 'transparent #t)
     (grob-interpret-markup grob
       (make-drawDashedLine-markup
        bend-line-thickness
        begin-x end-x line-y)
       ;; with 2.17.21 one could use:
       ;(make-translate-markup (cons 0 line-y)
       ;  (make-override-markup '(on . 0.3)
       ;    (make-draw-dashed-line-markup
       ;      (cons end-x 0))))
       )))

%%% music functions

bendOn =
#(define-music-function (parser location note) (ly:music?)
#{
  \override Voice.Slur #'stencil = #slur::draw-pointed-slur
  \override TabVoice.Slur #'stencil = #slur::draw-bend-arrow
  $note \noBreak
#})

bendOff = {
  \revert Voice.Slur #'stencil
  \override TabVoice.Slur #'stencil = #slur::draw-tab-slur
}

bendGrace =
#(define-music-function (parser location note) (ly:music?)
   #{
     \once \override Voice.Stem #'stencil = #point-stencil
     \once \override Voice.Flag #'stencil = ##f
     \once \override Voice.Stem #'direction = #DOWN
     \once \override Voice.Slur #'direction = #UP
     \grace #note
   #})

preBendHold =
#(define-music-function (parser location note) (ly:music?)
   #{
     \once \override TabVoice.Slur #'stencil = #slur::draw-pre-bend-only
     \once \override TabStaff.ParenthesesItem #'transparent = ##t
     <>\noBeam \parenthesize #note
   #})

preBendRelease =
#(define-music-function (parser location note) (ly:music?)
   #{
     \once \override TabVoice.Slur #'stencil = #slur::draw-pre-bend-hold
     \once \override TabStaff.ParenthesesItem #'transparent = ##t
     \once \override Voice.Slur #'direction = #DOWN
     <>\noBeam \parenthesize #note
   #})

holdBend =
#(define-music-function (parser location) ()
   #{
     \once \override TabVoice.Tie #'stencil = #tie::draw-hold-bend
   #})

shiftBend = {
  \once \override TabVoice.Slur #'stencil = #slur::draw-shifted-bend-arrow
}
