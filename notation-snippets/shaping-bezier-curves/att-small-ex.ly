
\version "2.17.29"
% with earlier versions, \attach ... Slur will probably affect only the first slur
\include "slur-attachments.ily"

\paper {
  ragged-right = ##t
}

\markup "\attach:"
{
  \slurUp
  \attach #'(stem head) Slur
  %\tweak positions #'(3 . 3) Slur
  a'2( c''
  \break
  a' g')
  
}

{
  \slurUp
  \attach 0.5 Slur
  %\tweak positions #'(3 . 3) Slur
  a'2( c''
  \break
  c'' g')
  
}

{
  \slurUp
  \attach stem Slur
  %\tweak positions #'(3 . 3) Slur
  a'2( c''
  \break
  a' g')
  
}