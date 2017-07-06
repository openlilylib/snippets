\version "2.19.50"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.pedal-decorations
%\include "./definitions.ily"

% example usage
pedalWithArrowsAndTextPed =
\override Score.PianoPedalBracket.after-line-breaking = #(pedalWithArrowsAndTextCallback "(ped)" #t)

pedalWithArrowsAndTextHalfPed =
\override Score.PianoPedalBracket.after-line-breaking = #(pedalWithArrowsAndTextCallback "½ ped" #t)

pedalWithArrowsAndTextThreeP =
\override Score.PianoPedalBracket.after-line-breaking = #(pedalWithArrowsAndTextCallback "3P" #t)

pedalWithArrowsAndTextUC =
\override Score.PianoPedalBracket.after-line-breaking = #(pedalWithArrowsAndTextCallback "UC" #t)

pedalWithTextPed =
\override Score.PianoPedalBracket.after-line-breaking = #(pedalWithArrowsAndTextCallback "(ped)" #f)

% example music

treble = {
  \clef treble
  \time 4/4
  c'4 c' c' c'
  c' c' c' c'
}

bass = {
  \clef bass
  \time 4/4

  c4
  \pedalWithArrowsAndTextHalfPed
  c\sustainOn c c
  c c c c
  \break
  c c c c
  \break
  c c c^\sustainOff\sustainOn c
  \break
  c c c c
  \break
  c c\sustainOff\sustainOn c c
  \break
  c c c c
  \break
  c c c c\sustainOff
  c c c c
  \break
  % turn off decorations
  \revert Score.PianoPedalBracket.after-line-breaking
  c c\sustainOn c c
  c c c c
  \break
  c c c\sustainOff c
  \pedalWithArrowsAndTextThreeP
  c\sustainOn c c\sustainOff
  c\sustainOn
  \break
  c c c c
  c c c c\sustainOff
  \break
  c
  \pedalWithArrowsAndTextUC
  c\sustainOn c c
  c c c\sustainOff
  \pedalWithTextPed
  c\sustainOn
  \break
  c c c c
  c c c\sustainOff c
}

\score {

  \new PianoStaff
  <<
    \new Staff = "treble"  { \treble }
    \new Staff = "bass" { \bass }
  >>

  \layout {
    \context {
      \Score
      pedalSustainStyle = #'bracket
    }
  }
}
