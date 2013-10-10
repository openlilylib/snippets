\version "2.16.2"

\header {
  title = "Color Explicit Voices"
  example-description = \markup {
    When this snippet is included it redefines all voiceXXX
    commands so explicitly assigned voices are colored.
    *Before* the include directive the configuration
    variables can be modified to configure the appearance.
    
    Please note that this debug mode doesn't behave well
    together with color-directions.ily
  }
}

#(define debug-explicit-voice-two-color red)

\include "color-voices.ily"

\relative g' {
  g a b c |
  d \voiceOne c 
  \voiceTwo bes8 as \voiceThree g\noBeam r
  \oneVoice
  e f g2.
}
