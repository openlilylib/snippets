
\version "2.17.29"
% with earlier versions, \attach ... Slur will probably affect only the first slur
\include "slur-attachments.ily"

\markup "\attach:"
{
  \slurUp
  \attach #'(stem "head") Slur
  <a c'>2( e')
  a'2( g')
  \slurDown
  a''2( g'')
  a''1( g'')
}