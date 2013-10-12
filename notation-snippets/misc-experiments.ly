\version "2.17.15"

\layout {
  ragged-right = ##t
  indent = #0
}

% this is a very crude test to check if i know how to make conditional statements
% based on strings passed as arguments to music function.

trololo =
#(define-music-function (parser location lst) (list?)
   (let* ((slurp (if (equal? "lol" (first (second lst)))
                     '(-5 . -3)
                     '(-3 . -5))))
     #{
       \once \override Slur #'positions = #slurp
     #}))

{
  % should override positions to (-5 . -3)
  \trololo #'(6 ("lol" "fooo"))
  g'1( a')
  \break
  % should override positions to (-3 . -5)
  \trololo #'(6 ("lala" "fooo"))
  g'1( a')
  \break
}