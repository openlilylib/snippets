\version "2.17.29"
% with earlier versions, \attach ... Slur will probably affect only the first slur
\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.shaping-bezier-curves.slur-attachments
%\include "slur-attachments.ily"

\paper {
  ragged-right = ##t
  indent = 0
}

\header {
  title = "Specifying slur end attachments"
}

\markup \vspace #2
\markup \justify {
  Slur attachment points may be specified using "\attach" function.
  When there is a stem on the same side of the notehead as the slur,
  the argument(s) will be interpreted as the fraction of the stem.
  If there are no stem to measure against, the numbers will be interpreted
  as an offset in staffspaces.  Positive values always point in the direction
  of the slur.  You can use "\"stem\"" and "\"head\"" shorthands.
  This function will NOT change slur direction.
}
\markup \vspace #0.5
\markup \justify {
  Of course, the most important feature of this function is that it's
  "\"transposition-proof\"" - one setting will give (almost) the same
  result for notes on different places on the staff.
}

\markup \vspace #1
\markup \typewriter "\attach #'(stem stem) Slur"
{
  \slurUp
  \attach #'(stem stem) Slur
  c'2( e')
  e'2( a')
  a'2( g')
  \slurDown
  a''2( g'')
  a''2( b')
}

\markup \typewriter "\attach stem Slur"
{
  \slurUp
  \attach stem Slur
  e'( e')
  a'2( a')
}

\markup \typewriter "\attach 0.5 Slur"
{
  \voiceOne
  \attach 0.5 Slur
  d''( d'')
  a''2( a'')
}

\markup \typewriter "\attach #'(head stem) Slur"
{
  \attach #'(head stem) Slur
  c'' ( f')
  e''( a')
  \slurUp
  e'( e')
  a'2( a')
}

\markup \typewriter "\attach #'(head 0.5) Slur"
{
  \attach #'(head 0.5) Slur
  c'' ( f')
  e''( a')
}

\markup \typewriter "\attach 2 Slur"
{
  \attach 2 Slur
  a'2( f')
  a''1( g'')
}

\markup \justify {
  The function won't get confused when stems point in different
  directions, even if "\"stem\"" attachment was requested. It will
  simply attach appropriate end to the notehead.
} \noPageBreak
\markup \typewriter "\attach stem Slur"
\noPageBreak
{
  \attach stem Slur
  c''2 ( f')
  e''( a')
}

\markup \justify {
  You can still control the distance between the slur end
  and the notehead - just use a numerical value:
}
\noPageBreak
\markup \typewriter "\attach #'(1.5 stem) Slur"
\noPageBreak
{
  \attach #'(1.5 stem) Slur
  c''2 ( f')
  e''( a')
}

\markup \justify {
  If you want to specify slurs very far from the notes (more than 4 staffspaces
  from the notehead), you have to tell LilyPond to perform more calculations
  by increasing region-size detail of the Slur:
}
\markup \typewriter "\attach 3 Slur"
{
  \slurUp
  \attach 3 Slur
  <>_\markup \tiny "default"
  e'( e')
  \override Slur.details.region-size = #8
  <>_\markup \teeny \typewriter "\override Slur.details.region-size = #8"
  e'( e')
  e''2
  e''2
  e''2
  e''2
  e''2
  e''2
  e''2
}

\markup \typewriter "\attach 1 Slur"
{
  \attach 1 Slur
  \voiceOne
  f'2( f')
  g''2 ( g'')
}

\markup \typewriter "\attach #'(0.5 1.5) Slur"
{
  \attach #'(0.5 1.5) Slur
  c'2 ( e')
  c''2 ( e'')
}

\markup \typewriter "\attach stem Slur"
{
  \slurUp
  \attach stem Slur
  a'2( c''
  \break
  a' g')
}
