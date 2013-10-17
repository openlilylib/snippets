\version "2.16.2"

\include "adjust-horizontal-spacing.ily"

\header {
  title = "Adjusting horizontal spacing"
  composer = "David Nalesnik, Janek Warchoł"
  % add comma-separated tags to make searching more effective:
  tags = "horizontal spacing, common shortest duration, common-shortest-duration,
  density, tightness"
  % is this snippet ready?  See meta/status-values.md
  status = "official"
}

  \markup {
    \vspace #2
    \wordwrap {
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
  }
\markup \vspace #2

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

\markup { \smaller \typewriter "\stretchHorizontalSpacing #-0.5" }
\new Staff {
  \stretchHorizontalSpacing #-0.5
  \music
}
\markup "Default spacing:"
\new Staff {
  \music
}

\markup { \smaller \typewriter "\stretchHorizontalSpacing #0.5" }
\new Staff {
  \stretchHorizontalSpacing #0.5
  \music
}

\markup { \smaller \typewriter "\horizontalSpacingLoose" }
\new Staff {
  \horizontalSpacingLoose
  \music
}

