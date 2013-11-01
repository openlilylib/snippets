\version "2.16.2"

spacingDensity =
#(define-music-function (parser location num) (number?)
   #{
     \newSpacingSection
     \override Score.SpacingSpanner #'common-shortest-duration =
     #(ly:make-moment num 1024 )
   #})
