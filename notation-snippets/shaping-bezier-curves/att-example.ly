
\include "slur-attachments.ily"

{
  \slurUp
  \attach #'(0.5 . 0.5) Slur
  a'1( c'2)
}
{
  <a c'>2-\attach #'(2 . 0.5) ( e')
  \transpose a c' { <a c'>2-\attach #'(0.7 . 0.5) ( e') }
}
\markup { This is so nice, not to have to worry about explicit positions! }
{
  \slurUp
  <a c'>2-\attach #'(0.7 . 0.5) ( e')
  \transpose a c' { <a c'>2-\attach #'(0.7 . 0.5) ( e') }
}

\markup { Again, this is so friendly: }
{
  \slurNeutral
  c''-\attach #'(0 . 0.8) ( f')
  \transpose c e' { c'-\attach #'(0 . 0.8) ( f) }
}

\markup { Is this really a '2'? What is the scale/measure? }
{
  \slurNeutral
  c''-\attach #'(0 . 2) ( f')
}

{
  \attach #'(1 . 1) Slur
  %\override Slur #'positions = #'(-6 . -3)
  c'2 ( e')
  c''2 ( e'')
}
\markup { What is the desired/expected meaning/behaviour of negative values? }
{
  \slurNeutral
  c''-\attach #'(-1 . 0) ( f')
}

\markup "Default:"
{ \slurUp <a c'>2( e') }

\markup "Positions override:"
{
  \slurUp 
  \override Slur #'positions = #'(1.0 . 1.7)
  d'2 ( e')
  <a c'>2( e')
}

\markup "\attach:"
{
  \slurUp
  <a c'>2-\attach #'(0 . 0) ( e')
}
{
  \slurUp
  c'2-\attach #'(-0.1 . -0.3) ( e')
}
{
  \slurUp
  d'2-\attach #'(1 . 1) ( e')
  e'2-\attach #'(1 . 0) ( e')
  f'2-\attach #'(0 . 0) ( f')
  \voiceOne
  g''2-\attach #'(1 . 1) ( g'')
}
{
  \slurUp
  <a c'>2-\attach #'(0.92 . 0.9) ( e')
}
{
  \slurUp
  <a c'>2-\attach #'(0.7 . 0.5) ( e')
}