\version "2.18.0"

\registerTemplate lalily.piano
#(define-music-function (parser location piece options)(list? list?)
   (let ((mods (assoc-get 'context-mods options #f #f))
         (rmods (assoc-get 'right-mods options #f #f))
         (lmods (assoc-get 'left-mods options #f #f))
         (dmods (assoc-get 'dynamic-mods options #f #f))
         (pmods (assoc-get 'pedal-mods options #f #f))
         (rclef (assoc-get 'right-clef options "G" #f))
         (lclef (assoc-get 'left-clef options "bass" #f))
         )
     #{
       \new PianoStaff \with {
         $(if (ly:context-mod? mods) mods)
         \override StaffGrouper.staff-staff-spacing =
         #'((basic-distance . 6)(minimum-distance . 1)(padding . 1)(stretchability . 4))
       } <<
         \new Staff = "right" \with {
           $(if (ly:context-mod? rmods) rmods)
           \consists \editionEngraver \musicPath right
         } <<
           \keepWithTag #'piano-right \getMusicDeep {} #'meta
           \keepWithTag #'piano-right { \getMusic {} global \getMusic right }
         >>
         \new Dynamics \with {
           $(if (ly:context-mod? dmods) dmods)
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
         } { \getMusic {} dynamics }
         \new Staff = "left" \with {
           $(if (ly:context-mod? lmods) lmods)
           \consists \editionEngraver \musicPath left
         } <<
           \keepWithTag #'piano-left \getMusicDeep {} #'meta
           \keepWithTag #'piano-left { \getMusic {} global \clef $lclef \getMusic left }
         >>
         \new Dynamics \with {
           $(if (ly:context-mod? pmods) pmods)
           \consists \editionEngraver $piece
           \override DynamicText.padding = #1
         } \getMusic {} pedal
       >>
     #}))

\registerTemplate lalily.piano.voice
#(define-music-function (parser location piece options)(list? list?)
   (let* ((verses (ly:assoc-get 'verses options #f #f))
          (voice-loc (assoc-get 'voice options '(voice) #f))
          (piano-loc (assoc-get 'piano options '(piano) #f))
          (voice-opts (assoc-set-all! options (get-default-options (create-music-path #f voice-loc) location)))
          (piano-opts (assoc-set-all! options (get-default-options (create-music-path #f piano-loc) location)))
          )
     (if (not (string? (assoc-get 'vocname voice-opts)))(set! voice-opts (assoc-set! voice-opts 'vocname "melody")))
     #{
       <<
         \callTemplate LY_ROOT.lalily.vocal $voice-loc #voice-opts
         \callTemplate LY_ROOT.lalily.piano $piano-loc #piano-opts
       >>
     #}))
