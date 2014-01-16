\version "2.16.2"

% TODO the hardcoded 1024 is very stupid.  One should use some other approach;
% maybe something like in `adjust-horizontal-spacing`. or take two arguments.
% or a fraction.
% should the newspacingsection be part of this command, actually?
% think about designing a coherent set of spacing commands.

spacingDensity =
#(define-music-function (parser location num) (number?)
   #{
     \newSpacingSection
     \override Score.SpacingSpanner #'common-shortest-duration =
     #(ly:make-moment num 1024 )
   #})
