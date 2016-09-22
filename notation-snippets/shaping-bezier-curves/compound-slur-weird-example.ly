\version "2.19.48"

% This has to be available
\include "notation-snippets/shaping-bezier-curves/compound-slurs.ily"

\paper {
  indent = 0
  ragged-last = ##f
  markup-system-spacing.minimum-distance = 15
  score-markup-spacing.minimum-distance = 12
  page-count = 1
}

\header {
  title = "Compound Slurs"
  subtitle = "Using openLilyLib/snippets"
}

weird=
#(define-event-function (ann? end-x end-y)((boolean?) number? number?)
   #{
     ^\compoundSlur \with {
       annotate = #ann?
       show-grid = #ann?
     start-ratio = 0.6
       end-point = #`(,end-x . ,end-y)
       
       inflection = 
       #'((X-ratio . .25)
          (Y-offset . 8)
          )
       
       inflection =
       #'((X-ratio . .15)
          (label . "A")
          )

       inflection = 
       #'((X-ratio . .5)
          (Y-offset . 11)
          )
       
       inflection = 
       #'((X-ratio . .65)
          (Y-offset . 8)
          (angle . -30)
          (ratio-left . .6)
          (label . "B")
          )
       
       inflection = 
       #'((X-ratio . .25)
          (Y-offset . -5)
          (angle . 45)
          )
       
       
     }
   #})
   
example = 
#(define-scheme-function (ann? end-x end-y)((boolean?) number? number?)
   #{
     \score {
       \new Staff 

       \relative c''' {
         c -\weird #ann? #end-x #end-y
         b a g f e d
         c b a g f e d
         c d e f g a b
         c d e f g a b )
       }
     }
   #})

\markup \vspace #9

\markup "Default"
\example ##t 0 0

\markup \vspace #7

\markup "Raise by 5"
\example 0 5

\markup "Raise by 15 and left by 30"
\example #-30 15

\markup "Lower by 10"
\example 0 #-10
