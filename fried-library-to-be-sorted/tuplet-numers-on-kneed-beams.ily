
% TODO: there may be a newer version of this somewhere.  Ask David Nalesnik.
% actually, i think he's working on a patch that would make Lily's default
% behaviour correct.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A function to position tuplet numbers next to kneed beams on a single
%% staff and between staves. Will ignore tuplets on ordinary beams and
%% with visible brackets.
%%
%% Usage: \override TupletNumber #'Y-offset = #kneed-beam
%%
%% You must use manual beaming for this function to work properly.
%%
%% An additional function, called with a separate override (see below), will
%% horizontally center the tuplet number on the kneed beam.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.17.3"

#(define ((kneed-beam length-fraction) tuplet-number)
   (let* ((bracket (ly:grob-object tuplet-number 'bracket))
          (first-note-col (ly:grob-parent tuplet-number X))
          (first-stem (ly:grob-object first-note-col 'stem))
          (beam (ly:grob-object first-stem 'beam)))

     (if (and (ly:grob? beam) ; beam on first note?
              (eq? #t (ly:grob-property beam 'knee)) ; is it kneed?
              (interval-empty? (ly:grob-extent bracket bracket Y))) ; no visible bracket?
         (let* ((stems (ly:grob-object beam 'stems))
                (stem-nearest-number (nearest tuplet-number stems))
                (first-stem-dir (ly:grob-property first-stem 'direction))
                (nearest-stem-dir (ly:grob-property stem-nearest-number 'direction))
                (beaming (ly:grob-property stem-nearest-number 'beaming))
                (beaming-near-number (if (car beaming) (car beaming) (cdr beaming)))
                (beam-multiplier
                 (if (= nearest-stem-dir UP)
                     (count positive? beaming-near-number)
                     (count negative? beaming-near-number)))
                (beam-X-pos (ly:grob-property beam 'X-positions))
                (beam-dx (- (cdr beam-X-pos) (car beam-X-pos)))
                (beam-Y-pos (ly:grob-property beam 'positions))
                (beam-dy (- (cdr beam-Y-pos) (car beam-Y-pos)))
                (beam-slope (/ beam-dy beam-dx))
                (mid-beam-Y (+ (car beam-Y-pos)
                              (* beam-slope length-fraction beam-dx)))
                (bracket-padding (ly:grob-property bracket 'padding))
                (beam-width (ly:grob-property beam 'beam-thickness))
                (beam-gap (* 0.5 (car (ly:grob-property beam 'beam-gap)))))

           (+ mid-beam-Y
             (* nearest-stem-dir
               (+ bracket-padding
                 (* 0.5 beam-width)
                 (if (= first-stem-dir nearest-stem-dir)
                     0
                     (* beam-multiplier (+ beam-gap beam-width)))))))

         (ly:tuplet-number::calc-y-offset tuplet-number))))

%% find the stem closest to the tuplet-number
#(define (nearest tuplet-number stems)
   (let* ((refp (ly:grob-system tuplet-number))
          (X-coord (interval-center (ly:grob-extent tuplet-number refp X)))
          (closest (ly:grob-array-ref stems 0)))
     (let lp ((x 1))
       (if (<= (abs (- X-coord
                      (ly:grob-relative-coordinate
                       (ly:grob-array-ref stems x) refp X)))
               (abs (- X-coord
                      (ly:grob-relative-coordinate closest refp X))))
           (set! closest (ly:grob-array-ref stems x)))
       (if (< x (1- (ly:grob-array-length stems)))
           (lp (1+ x))
           closest))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A function which horizontally centers a tuplet number on a kneed beam.  May
%% be used in conjunction with the earlier function.
%%
%% Usage: \override  TupletNumber #'X-offset = #center-on-beam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define ((center-on-beam length-fraction) tuplet-number)
   (let* ((bracket (ly:grob-object tuplet-number 'bracket))
          (first-note-col (ly:grob-parent tuplet-number X))
          (first-stem (ly:grob-object first-note-col 'stem))
          (beam (ly:grob-object first-stem 'beam)))

     (if (and (ly:grob? beam)
              (eq? #t (ly:grob-property beam 'knee))
              (interval-empty? (ly:grob-extent bracket bracket Y)))

         (let* ((refp (ly:grob-system tuplet-number))
                (beam-Y-pos (ly:grob-property beam 'positions))
                (number-X (interval-center (ly:grob-extent tuplet-number refp X)))
                (beam-extent (ly:grob-extent beam refp X))
                (beam-center-X (+ (car beam-extent)
                                 (* length-fraction
                                   (interval-length beam-extent)))))

           (- beam-center-X number-X))

         (ly:tuplet-number::calc-x-offset tuplet-number))))

correctTupletNumber =
#(define-music-function (parser location pos) (number?)
   (set! pos (max 0 pos))
   (set! pos (min 1 pos))
   #{
     \override TupletNumber #'X-offset = #(center-on-beam pos)
     \override TupletNumber #'Y-offset = #(kneed-beam pos)
   #})

correctTupletNumberDefault = \correctTupletNumber #0.5

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{
  \markup "when beam positions are overridden, the tuplet override gets applied also for non-kneed beams:"
  \markup \huge \bold "FIXED :)"
  \new Staff \relative f' {
  \override TupletBracket #'direction = #UP
  \times 2/3 { a8 c e }
  \once \override Beam #'positions = #'(-4 . -1)
  \times 2/3 { a,8 c e }

  \correctTupletNumber #0.2
  \times 2/3 { a,8 c e }
  \once \override Beam #'positions = #'(-4 . -1)
  \times 2/3 { a,8 c e }
  }

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  top = \change Staff = "1"
  bottom = \change Staff = "2"

  \score {
  \new PianoStaff <<
  \new Staff = "1" {
  s1^"Before:"
  s1^"After:"
  }
  \new Staff = "2" {
  \clef bass
  \relative c {
  \override Beam #'auto-knee-gap = #1

  %% UNCORRECTED
  \set tupletSpannerDuration = #(ly:make-moment 1 4)
  \times 2/3 {
  \bottom c8.[ g'16 \top e'8]
  c'8.[ e,16 \bottom g,8]
  \top e''8.[ \bottom c,,16 \top g''8]
  }
  \times 4/5 {
  \bottom c,,16[ \top g''16. e'32 \bottom g,,16 \top c'']
  }

  %% CORRECTED
  \set tupletSpannerDuration = #(ly:make-moment 1 4)
  \correctTupletNumberDefault %%%%%%%%%%
  \times 2/3 {
  \bottom c,,,8.[ g'16 \top e'8]
  \once \correctTupletNumber #0.3 %%%%%%%%%%%
  c'8.[ e,16 \bottom g,8]
  \top e''8.[ \bottom c,,16 \top g''8]
  }
  \times 4/5 {
  \bottom c,,16[ \top g''16. e'32 \bottom g,,16 \top c'']
  }
  }
  \bar "||"
  }
  >>
  }
%}