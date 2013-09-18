\version "2.17.25"

\header {
  snippet-title = "Adjusting horizontal spacing"
  snippet-author = "David Nalesnik, Janek Warchoł"
  snippet-description = \markup {
    Sometimes you want to change horizontal spacing relative to what
    LilyPond calculated by default.  You could do this by overriding
    common-shortest-duration property of the SpacingSpanner, but that
    requires some tial-and-error to figure out what values make the
    spacing tighter, and what values make it looser.  Additionally,
    this is different for different pieces - in one piece overriding
    common-shortest-duration to 1/8 may make the spacing looser than
    the default, but in another piece it may make the spacing tighter.

    This function allows you to adjust spacing without having to
    figure out anything.  Positive values make spacing looser,
    negative make it tighter, working similarly in any score.
    There are two predefined “shorthands”.

    This can also be used inside a "\layout" block.
  }
  % add comma-separated tags to make searching more effective:
  tags = "horizontal spacing, common shortest duration"
  % is this snippet ready?  See meta/status-values.md
  status = "official"
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


%%%%%%%%%%%%%%%%%%%%%%%%%%
% usage example:         %
%%%%%%%%%%%%%%%%%%%%%%%%%%

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
