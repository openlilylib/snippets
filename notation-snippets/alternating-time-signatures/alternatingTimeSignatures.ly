\version "2.18.2"

alternatingTimeSignatures =
#(define-music-function (parser location timesigs)(list?)
   (_i "Print alternating time signatures. The argument is a Scheme list
of lists. Each list describes one fraction to be printed.
When the function has executed the first of the given time signatures
will be the effective @code{\\time}, while the following are simply graphical.
When using it you will want to use @code{\\omit Score.TimeSignature}
before the next use of @code{\\time}. Please note that this does not
perform any checks regarding the used time signatures, so you're
responsible yourself to write consistent music. To return to normal
use of time signatures use @code{\\undo \\omit Score.TimeSignature}.")
   (let ((firstsig (cons
                    (caar timesigs)
                    (cadar timesigs))))
     #{ \once\override Staff.TimeSignature.stencil =
        #(lambda (grob)
           (grob-interpret-markup grob
             #{ \markup \override #'(baseline-skip . 0)
                \number
                #(map (lambda (x) #{ \markup \column #(map number->string x) #})
                   timesigs)
             #}))
        \time #firstsig #}))


\relative c' {
  \alternatingTimeSignatures #'((3 8) (4 8) (5 8))
  c8 d e
  \omit Score.TimeSignature
  \time 4/8
  f g a b
  \time 3/8
  c g c,
  d e f
  g a b
  \time 4/8
  c b a b
  \undo \omit Score.TimeSignature
  \time 5/8
  c b a g f

}

\relative c' {
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \alternatingTimeSignatures #'((3 4) (4 7))
  c4 d e
  \omit Score.TimeSignature
  \time 4/7
  \tupletSpan 4*16/7
  \tuplet 7/4 {
    f e f fis
    g a ais b
  }
  \time 3/4
  c4 g c,
}