\version "2.19.6"

% Goulds recommendation for interchangeable metres
% with one of her examples.

add-interch-ts =
#(define-scheme-function (parser location ic-ts)
   (list?)
   (lambda (grob)
     (ly:stencil-combine-at-edge
      (ly:time-signature::print grob) 0 1
      (parenthesize-stencil
       (grob-interpret-markup grob
         #{ \markup \number \override #'(baseline-skip . 0)
            \column { #(map number->string ic-ts) } #})
       0.1 0.4 0.4 0.1) 0)))

\relative c'' {
  \time 6/8
  c4. c8 c c
  \once\override Staff.TimeSignature.stencil = \add-interch-ts #'(3 4)
  \time 9/8
  c4. c8 c c c4 c8
  \omit Score.TimeSignature
  \time 3/4
  c8 c c c c4
}

