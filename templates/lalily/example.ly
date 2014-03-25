\version "2.18.0"
\include "definitions.ily"

\registerTemplate mein.test
#(define-music-function (parser location piece opts)(list? list?)
   #{
     \getMusic music
   #})


\setDefaultTemplate musik.test mein.test #'()
\putMusic music \relative c'' {
  bes4 a c b
}

\lalilyTest
