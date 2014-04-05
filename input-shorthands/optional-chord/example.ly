\version "2.16.2"

\include "./definitions.ily"

<<
  \new ChordNames \chordmode {
    b2:m \optionalChord g4 \optionalChord d e1:m
  }
  \new Staff {
    <fis' b' d''>2
    <g' b' d''>4
    <fis' a' d''>
    <e' g' b'>1
  }
>>
