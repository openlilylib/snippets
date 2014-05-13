\version "2.19.6"

gould-irreg-alt-metres =
#(define-scheme-function (parser location timesignI timesignII) 
   (list? list?)
   (lambda (grob)
     (grob-interpret-markup grob
       #{ \markup \number \override #'(baseline-skip . 0)
          {
            \column { #(map number->string timesignI) }
            \override #'(thickness . 3.6)
            \draw-line #'(-1.1 . 0)
            \column { #(map number->string timesignII) }
       } #})))

\relative c'' {
  \once\override Staff.TimeSignature.stencil = 
  \gould-irreg-alt-metres #'(5 8) #'(6 8)
  c1
}

