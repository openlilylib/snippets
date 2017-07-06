\version "2.16.2"

\include "oll-core/package.ily"
\loadModule snippets.debugging-layout.color-voices-and-directions.color-voices
%\include "color-voices.ily"

#(define debug-explicit-voice-two-color red)

\relative g' {
  g a b c |
  d \voiceOne c
  \voiceTwo bes8 as \voiceThree g\noBeam r
  \oneVoice
  e f g2.
}
