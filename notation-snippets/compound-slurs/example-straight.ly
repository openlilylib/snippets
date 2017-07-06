\version "2.19.48"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.compound-slurs
%\include "compound-slurs.ily"


\paper {
  indent = 0
  ragged-last = ##f
  markup-system-spacing.minimum-distance = 25
  score-markup-spacing.minimum-distance = 12
  page-count = 1
}

\header {
  title = "Compound Slurs With Straight Segments"
  subtitle = "Using openLilyLib/snippets"
}

straight =
^\compoundSlur \with {
%  annotate = ##t
  %show-grid = ##t
  
  start-angle = 60
  end-angle = 60
  start-ratio = 0.25
  end-ratio = 0.3
  inflection =
  #'((X-ratio . .15)
     (Y-offset . 10)
     (ratio-left . .5)
     (angle . straight)
     )
  

  inflection =
  #'((X-ratio . .5)
     (Y-offset . 10)
     (ratio-right . .8)
     (angle . 0)
     )

  inflection =
  #'((X-ratio . .6)
     (Y-offset . 0)
     (ratio-left . .5)
;     (angle . straight)
     )

  inflection =
  #'((X-ratio . .4)
     (Y-offset . -20)
     (angle . 0)
     )
}


music = \relative c''' {
  c -\straight
  b a g f e d
  c b a g f e d
  c d e f g a b
  c d e f g a b )
}

\score {
  \new Staff \music
}