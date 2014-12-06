\version "2.19.13"

\include "./definitions.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This example shows how "Scale vertical spacing" can be used.
% The implementation is in the file `definitions.ily`.
% The documentation is in the file `README.md`.

% scale all page layout variables by the same amount

\scaleVerticalSpacingPageLayout #1.5

% scale all "in-system" properties by the same amount,
% except for Lyrics, which is scaled by a different amount

\scaleVerticalSpacingInSystems
#'((all 1.5)
   (Lyrics 1.15))


% EXAMPLE MUSIC

global = { \key c \major \time 4/4 }
somenotes = \repeat unfold 48 { c8[ c] }
chordNames = \chordmode { \global \repeat unfold 12 { c1:m } }
melody = \relative c'' { \global \somenotes }
verse = \lyricmode { \repeat unfold 48 { la la } }
right = \relative c'' { \global \somenotes }
left = \relative c' { \global \somenotes }

\score {
  <<
    <<
      \new ChordNames \chordNames
      \new Staff { \melody }
      \addlyrics { \verse }
    >>
    \new PianoStaff <<
      \new Staff = "right" { \right }
      \new Staff = "left" { \clef bass \left }
    >>
  >>
  \layout { }
}
