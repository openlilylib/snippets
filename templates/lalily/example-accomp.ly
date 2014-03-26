\version "2.18.0"
\include "example-satb.ly"

% TODO comments

\registerTemplate chor-accomp
#(define-music-function (parser location piece options)(list? list?)
   #{
     <<
     \clralist acc-trmp
     \addalist acc-trmp staff-mods \with { instrumentName = "Trumpet (Bb)" }
     \addalist acc-trmp input-concert-pitch ##t
     \addalist acc-trmp output-concert-pitch ##f
     \callTemplate LY_ROOT.lalily.instrument.trumpet trumpet #acc-trmp
     
     \clralist choir
     \addalist choir staff-mods \with { midiInstrument = "choir aahs" }
     \createScoreWithOptions LY_UP #choir
     
     \clralist acc-pno
     \callTemplate LY_ROOT.lalily.piano piano #acc-pno
     >>
   #})

#(set-global-staff-size 16)

\setDefaultTemplate \musicPath accomp chor-accomp #'()
\inheritAllHeaders LY_UP
\setPaper \paper {
  ragged-bottom = ##f
  ragged-last-bottom = ##f
}
\setMidi \midi {
  \tempo 4=120
}

\putMusic LY_UP.meta { \numericTimeSignature \getMusic LY_UP.meta }

\putMusic trumpet \relative c'' {
  r4. c8 a4. bes8 | c4 d2 e4 | f8 e d c a4 g | f4.. f'16 f4 e8 d | c1 |
}
\putMusic piano.right { \set Staff.printPartCombineTexts = ##f \partcombine \getMusic LY_UP.sop.music \getMusic LY_UP.alt.music }
\putMusic piano.left { \set Staff.printPartCombineTexts = ##f \partcombine \getMusic LY_UP.ten.music \getMusic LY_UP.bas.music }

\lalilyTest
