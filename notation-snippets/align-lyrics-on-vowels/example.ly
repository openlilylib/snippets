\version "2.16.2" % absolutely necessary!

\include "definitions.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%
% usage example:         %
%%%%%%%%%%%%%%%%%%%%%%%%%%

chant =

\relative c'' {

  c4 c4 c4 c4 c4 c4 c4 c4 c4 a2 \bar "|"
  c4 c4 c4 \break

  c4 c4 c4 c4
  a4 a2 \bar "|"
  c4 c4 c4 c4 c4 c4 c4 c2 \break

  c4 b4 a4 c4 c2 \bar "|"
  c4 c4 c4 c4 c4 c4 c4 c2 \bar "|"
  \break

}

words = \lyricmode {
  \set stanza = "℣." Lat mig pri -- sa dig he -- li -- ga Jung -- fru. \set stanza = "℟." Ge mig kraft
  mot di -- na fi -- en -- der.
  \set stanza = "℣." Hell dig Mar -- i -- a, full av nad, Herren _ ar med dig
  \set stanza = "℟." Val -- sign -- ad ar du bland kvin -- nor,
}

\score {
  \new Staff

  <<
    \new Voice = "melody" \chant
    \new Lyrics \lyricsto "melody" \words
  >>

  \layout {

    \context {
      \Score
      \remove "Time_signature_engraver"
      timing = ##t
      \override Stem #'transparent = ##t
    }

    \context {
      \Staff
      \override StaffSymbol #'line-count = #4
      \remove "Time_signature_engraver"
      \remove "Bar_engraver"
    }
    \context {
      \Voice
      \override Stem #'length = #0
      \override TextScript #'font-shape = #'italic
      \override TextScript #'font-series = #'bold
    }

    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-vowel
    }
  }
}
