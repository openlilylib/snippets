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
  <<
    \tag#'score { s_\markup\italic "balance with winds" }
    \sharedStems
      { a'4 f' a' g' }
      { f' d' f' e' }
  >>
  \voiceDivisi {
    g' b' d' b'
    d'' b' g' b'-\tag#'part \upbow
  }
  {
    g'4 e' g' e'
    b' g' b' g'-\tag#'part \upbow
  }
  <<
    \tag#'editorial {s4\< s s s\f }
    \tag#'score { s_\markup\italic "balance " }
    \staffDivisi
      { <a' f''> g' q b' }
      { a' <e' b' e''> a' q }
  >>
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
} \keepWithTag divI.editorial.part \music

\markup \bold { Violin II part: }
\new Staff \with {
  instrumentName = "Violin II"
  shortInstrumentName = "V II"
} \keepWithTag divII.editorial.part \music

\markup \bold { Combined: }
\keepWithTag editorial.score \divisibleStaff "Violin" \music
