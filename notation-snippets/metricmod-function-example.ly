\version "2.17.26"

\include "metricmod-function.ily"

\header {
  title = "Metric Modulation Function"
  composer = "Peter Bjuhr"
  
  % add comma-separated tags to make searching more effective:
  tags = "metric modulation, tempo modulation, metric relationship, 
  metric relationship, tempo equation, metric equation"
  % is this snippet ready?  See meta/status-values.md
  status = "unknown"
  % Thoughts about status and improvements:
  % Are the unicode symbols a problem? 
  % Maybe the arrows need a little padding!?
}

\markup {
  \vspace #2
  \wordwrap {
    Function that creates a markup for metric modulation.
    The user inputs:
    Note-value (uses note-by-number; 1=half,2=quarter,3=8th,4=16th...)
    Number of dots (0 for no dot, 2 for double dot)
    Tuplet value (0 for no tuplet, 3 for triplet)
    for each side of the metric equation 
    (the arguments of the function is thus 6 numbers)   
  }
}
\markup \vspace #2

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%
{
  \time 6/8
  c'8 d' e' d'4. |
  \mark \markup \metr-mod #2 #1 #0 #2 #0 #0 
  \time 2/4
  e'8 d' f' g' c''2 
}
{
  c'2 \tuplet 3/2 { d'4 e' d' } |
  \mark \markup \metr-mod #2 #0 #3 #3 #0 #0
  \time 6/8
  e'8 d' f' g' c''4 
}
\relative c'{
  \time 2/2
  c8 d e f \tuplet 5/4 {cis d e fis e} |
  \mark \markup \metr-mod #3 #0 #5 #3 #0 #3
  \time 6/8
  \tuplet 3/2 { dis e g } fis8 g4.
}
