\version "2.18.2"

\header {
  snippet-title = "Interchangeable metres"
  snippet-author = "Peter Bjuhr"
  snippet-description = \markup {
    Implementation of alternatives for interchangeable metres
    with exemples (Gould: Behind Bars p 174). 
  }
  % add comma-separated tags to make searching more effective:
  tags = "time signature"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2/4=6/8
add-interch-ts-eqsg =
#(define-scheme-function (parser location ic-ts)
   (list?)
   (lambda (grob)
     (ly:stencil-combine-at-edge
      (ly:time-signature::print grob) 0 1
      (grob-interpret-markup grob
        #{ \markup {
          \override #'(baseline-skip . 0) \number {
            \hspace #0.2
            \general-align #Y #0.1 { = }
            \hspace #-0.5
            \column { #(map number->string ic-ts) }
          }
           }
        #})
      0 )))

% 2/4(6/8)
add-interch-ts-prnt =
#(define-scheme-function (parser location ic-ts)
   (list?)
   (lambda (grob)
     (ly:stencil-combine-at-edge
      (ly:time-signature::print grob) 0 1
      (parenthesize-stencil
       (grob-interpret-markup grob
         #{ \markup {
           \override #'(baseline-skip . 0) \number {
             \hspace #0.1
             \column { #(map number->string ic-ts) }
           }
            }
         #})
       0.1 0.4 0.4 0.1) 0)))

% 2/4/6/8
add-interch-ts-dsh =
#(define-scheme-function (parser location ic-ts)
   (list?)
   (lambda (grob)
     (ly:stencil-combine-at-edge
      (ly:time-signature::print grob) 0 1
      (grob-interpret-markup grob
        #{ \markup {
          \override #'(baseline-skip . 0) \number {
            \hspace #0.5
            \general-align #Y #0 { / }
            \column { 6 8}
          }
           }
        #})
      0 )))