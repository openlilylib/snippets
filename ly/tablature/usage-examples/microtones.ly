\version "2.18.2"

\include "openlilylib"

\useLibrary Tablature
\useModule tablature.microtones

% Hack needed until issue #136 is fixed:
% https://github.com/openlilylib/openlilylib/issues/136
#(ly:message "loaded")

mus = {
  geses
  geseh
  ges
  geh
  g
  gih
  gis
  gisih
  gisis
}

test-music =
\relative eeh, {
  \cadenzaOn
  <>^"E"
  \transpose g e,
  %% exclude `eeses', `eeseh' and `ees' for current string-tuning
  $(make-sequential-music
    (drop (ly:music-deep-copy (ly:music-property mus 'elements)) 3))

  <>^"F" \bar "||"
  \transpose g f,
  %% exclude `feses' for current string-tuning
  $(make-sequential-music
    (drop (ly:music-deep-copy (ly:music-property mus 'elements)) 1))

  <>^"G" \bar "||" \transpose g g, \mus
  <>^"A" \bar "||" \transpose g a, \mus
  <>^"B" \bar "||" \transpose g b, \mus
  <>^"C" \bar "||" \transpose g c \mus
  <>^"D" \bar "||" \transpose g d \mus
  <>^"E" \bar "||" \transpose g e \mus
  <>^"F" \bar "||" \transpose g f \mus
  <>^"G" \bar "||" \transpose g g \mus
  <>^"A" \bar "||" \transpose g a \mus
  <>^"B" \bar "||" \transpose g b \mus
  <>^"C" \bar "||" \transpose g c' \mus
  <>^"D" \bar "||" \transpose g d' \mus
  <>^"E" \bar "||" \transpose g e' \mus
  <>^"F" \bar "||" \transpose g f' \mus
  <>^"G" \bar "||" \transpose g g' \mus
  <>^"A" \bar "||" \transpose g a' \mus
  <>^"B" \bar "||" \transpose g b' \mus
  \bar "||"
}

custom-tuning = \stringTuning <eeh, a, d ges beh eeh'>

<<
  \new Staff << \clef "G_8" \test-music >>
  \new TabStaff \with { stringTunings = \custom-tuning } \test-music
>>
