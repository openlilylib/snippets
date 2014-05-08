

\version "2.19.6"
#(define ((custom-time-signature one two) grob)
   ;; derived from
   ;; http://lilypond.1069038.n5.nabble.com/Mixed-Time-Signatures-Non-regular-alternantion-between-5-8-and-8-8-td18617.html
   (let ((numOne (number->string (car one)))
          (denOne (number->string (cdr one)))
          (numTwo (number->string (car two)))
          (denTwo (number->string (cdr two))))
   (grob-interpret-markup grob
     (markup #:override '(baseline-skip . 0) #:number
;TODO: #:line is a list of columns,
; so it's easy to extend this with more items.
; will take a list of pairs, convert all items to strings
; and create the list of #:column items.
; Then simply pass this list to #:line
       (#:line ((#:column (numOne denOne))
                (#:column (numTwo denTwo))))))))


alternatingTimeSignatures =
#(define-music-function (parser location timesigs)(list?)
   (_i "Print alternating time signatures. The argument is a Scheme list
of (currently two) pairs. Each pair describes one fraction to be printed.
When the function has executed the first of the given time signatures
will be the effective @code{\\time}, while the second is simply graphical.
When using it you will have to use @code{\\omit Score.TimeSignature}
before the next use of @code{\\time}. Please note that this does not
perform any checks regarding the used time signatures, so you're
responsible yourself to write correct music. To return to normal
use of time signatures use @code{\\revert Score.TimeSignature.stencil}.")
   (let* ((sigOne (car timesigs))
          (sigTwo (cadr timesigs)))
     #{
       \override Score.TimeSignature.stencil =
         #(custom-time-signature sigOne sigTwo)
       \time #sigOne
     #}
     ))

\relative c' {
  \alternatingTimeSignatures #'((3 . 8) (4 . 8))
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
  \revert Score.TimeSignature.stencil
  \time 5/8
  c b a g f

}

\relative c' {
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \alternatingTimeSignatures #'((3 . 4) (4 . 7))
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