\version "2.17.26"

\include "definitions.ily"

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%
{
  \time 6/8
  c'8 d' e' d'4. |
  \mark \markup \metr-mod #"4." #0 #"4" #0
  \time 2/4
  e'8 d' f' g' c''2
}
{
  c'2 \tuplet 3/2 { d'4 e' d' } |
  \mark \markup \metr-mod #"4" #3 #"8" #0
  \time 6/8
  e'8 d' f' g' c''4
}
\relative c'{
  \time 2/2
  c8 d e f \tuplet 5/4 {cis d e fis e} |
  \mark \markup \metr-mod #"8" #5 #"8" #3
  \time 6/8
  \tuplet 3/2 { dis e g } fis8 g4.
}
