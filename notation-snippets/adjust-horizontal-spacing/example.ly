\version "2.16.2"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.adjust-horizontal-spacing
%\include "definitions.ily"

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
  \horizontalSpacingLoose
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

