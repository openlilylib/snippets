\version "2.16.2"

\include "./display-directions.ily"

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%

\new Staff {
  c''^(
  d'')
  c''(__
  d'')
  c''_\(
  d''\)
  <c'' e''>^(_\mf
  <d'' f''>)
  c'~ c'^~ c'_~ c'
}

\layout {
  \context {
    \Voice
    \consists #Color_explicit_direction_engraver
  }
}
