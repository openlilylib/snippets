\version "2.16.2"

\header {
  snippet-title = "Interval Brackets for Scales"
  snippet-author = "Joram Berger and Thomas Morley"
  snippet-source = "http://lists.gnu.org/archive/html/lilypond-user/2013-03/msg00753.html"
  snippet-description = \markup {
    This snippet provides brackets for semitones, tones and
    one and a half tones for scales as used mostly for
    educational purposes. The functionality is turned on via
    "\scaleSettings" and turned off via "\revertScaleSettings".
    Three brackets are provided: "\semitone", "\tone" and
    "\threesemitone".
  }
  tags = "scale, interval, semitone, bracket"
  status = "ready"
}

%{
  This can be further improved with ideas in the mentioned thread
  and otherwise. Currently it only works for rising scales.
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define ((elbowed-glissando coords) grob)

   (define (pair-to-list pair)
     (list (car pair) (cdr pair)))

   (define (normalize-coords goods x y)
     (map
      (lambda (coord)
        (cons (* x (car coord)) (* y (cdr coord))))
      goods))

   (define (my-c-p-s points thick)
     (make-connected-path-stencil
      points
      thick
      1.0
      1.0
      #f
      #f))

   ; outer let to trigger suicide
   (let ((sten (ly:line-spanner::print grob)))
     (if (grob::is-live? grob)
         (let* ((thick (ly:grob-property grob 'thickness 0.1))
                (xex (ly:stencil-extent sten X))
                (lenx (interval-length xex))
                (yex (ly:stencil-extent sten Y))
                (xtrans (car xex))
                (ytrans (car yex))
                (uplist
                 (map pair-to-list
                   (normalize-coords coords lenx 3)))
                (downlist
                 (map pair-to-list
                   (normalize-coords coords lenx -3))))

           (ly:stencil-translate
            (my-c-p-s uplist thick)
            (cons xtrans ytrans)))
         '())))

#(define semi-tone-gliss
   (elbowed-glissando '((0.5 . -0.5) (1.0 . 0.15))))

#(define tone-gliss
   (elbowed-glissando '((0 . -0.5) (1.0 . -0.35) (1.0 . 0.16))))

#(define three-semi-tone-gliss
   (elbowed-glissando '((0 . -0.35) (0.5 . -0.7) (1.0 . -0.2) (1.0 . 0.15))))

intervalBracketsOn = {
  \override Glissando #'Y-offset = #-1
  \override Glissando #'thickness = #0.2
  \override Glissando #'bound-details =
  #'((left    (padding . 0.2))
     (right   (end-on-accidental . #f) (padding . 0.2)))
}

intervalBracketsOff = {
  \revert Glissando #'Y-offset
  \revert Glissando #'thickness
  \revert Glissando #'bound-details
}


semiTone =
#(define-event-function (parser location)()
   #{
     \tweak #'stencil #semi-tone-gliss
     \glissando
   #})

tone =
#(define-event-function (parser location)()
   #{
     \tweak #'stencil #tone-gliss
     \glissando
   #})

threeSemiTone =
#(define-event-function (parser location)()
   #{
     \tweak #'stencil #three-semi-tone-gliss
     \glissando
   #})
