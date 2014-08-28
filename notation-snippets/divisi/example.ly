\version "2.19.13"

\include "definitions.ily"

\layout {
  short-indent = 2\cm
  indent = 2\cm
  line-width = 10\cm
}

music = {
  \together
  c'4 d' e' f'
  \sharedStems
  { a' f' a' g' }
  { f' d' f' e' }
  \voiceDivisi {
    g' b' d' b'
    d'' b' g' b'
  }
  {
    g'4 e' g' e'
    b' g' b' g'
  }
  \staffDivisi
  { <a' f''> g' q b' }
  { a' <e' b' e''> a' q }
  \together
  c' d' e' f'
  \soloI { g' b' d' b' }
  \together
  <f' a'> <d' f'> <f' a'> <e' g'>
  f' e' d' c'
}

\markup \bold { Violin I part: }
\new Staff \with {
  instrumentName = "Violin I"
  shortInstrumentName = "V I"
} \keepWithTag divI \music

\markup \bold { Violin II part: }
\new Staff \with {
  instrumentName = "Violin II"
  shortInstrumentName = "V II"
} \keepWithTag divII \music

\markup \bold { Combined: }
\divisibleStaff "Violin" \music
