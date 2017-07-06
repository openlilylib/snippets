\version "2.19.48"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.compound-slurs
%\include "compound-slurs.ily"


\paper {
  indent = 0
  ragged-last = ##f
  markup-system-spacing.minimum-distance = 35
}

\header {
  title = "Compound Slurs"
  subtitle = "Flat slurs using openLilyLib/snippets"
}

flatSlur =
\compoundSlur \with {
       show-grid = ##f
       start-angle = 45
       end-angle = -45

       inflection =
       #'((X-ratio . .1)
          (Y-offset . 5)
          )

       inflection =
       #'((X-ratio . .9)
          (Y-offset . 5)
          (angle . 0)
          )
     }

\relative c'' {
  c \flatSlur
   g' g g
   \repeat unfold 12 g
   g g g c, )
}
