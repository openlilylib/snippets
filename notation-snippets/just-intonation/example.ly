\version "2.19.32"

\include "definitions.ily"

{
  % provide a duration and a ratio
  \ji d'4 3/2 c' f' g'
  % provide only a ratio
  \ji f' 9/16 e' d' e'
  % provide cent values
  \ji d' 30.5 e' f' e'
  % Use \ji inside chords
  <\ji d' #28
   g'
   \ji c'' 9/5>
}
