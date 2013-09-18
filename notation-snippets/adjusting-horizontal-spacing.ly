\version "2.17.25"

\header {
  snippet-title = "Adjusting horizontal spacing"
  snippet-author = "David Nalesnik, Janek Warchoł"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "horizontal spacing, common shortest duration"
  % is this snippet ready?  See meta/status-values.md
  status = "undocumented"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

stretchHorizontalSpacing =
#(define-music-function (parser location exponent) (number?)
   (_i "This function determines the default value of the property
@var{common-shortest-duration} and multiplies it by a moment
derived from the @var{exponent} passed as an argument.
Negative values of @var{exponent} make the spacing tighter,
while positive values make the spacing looser.
")
   #{
     \override Score.SpacingSpanner.common-shortest-duration =
     #(lambda (grob)
        (let* ((func (assoc-get 'common-shortest-duration
                       (reverse (ly:grob-basic-properties grob))))
               (default-value (func grob))
               ;; When dealing with moments, we need to operate on an
               ;; exponential scale. We use 'inexact->exact' to make sure
               ;; that 'rationalize' will return an exact result as well.
               (factor (inexact->exact (expt 2 (- 0 exponent))))
               ;; The second argument to 'rationalize' has to be fairly
               ;; small to allow lots of stretching/squeezing.
               (multiplier (ly:make-moment (rationalize factor 1/2000))))
          (ly:moment-mul default-value multiplier)))
   #})

horizontalSpacingLoose = \stretchHorizontalSpacing #1
horizontalSpacingTight = \stretchHorizontalSpacing #-1

music = \relative c {
  \clef "bass"
  \key d \minor
  \time 3/4
  \mergeDifferentlyDottedOn
  <<
    { \slurDashed d8.-\flageolet( e16) e4.-\trill( d16 e) }
    \\
    { d4_2 a2 }
  >>
  \slurDashed
  <f' a, d,>4. e8( d c)
  \slurSolid
  bes8 g' f e16( f g_1 a_2 bes_3 d,_2)
}

\markup { \smaller \typewriter "\horizontalSpacingTight" }
\new Staff {
  \horizontalSpacingTight
  \music
}

\markup "Default spacing:"
\new Staff {
  \music
}

\markup { \smaller \typewriter "\horizontalSpacingLoose" }
\new Staff {
  \horizontalSpacingLoose
  \music
}
