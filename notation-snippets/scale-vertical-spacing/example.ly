\version "2.19.13"

\include "./definitions.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This example shows how "Scale vertical spacing" can be used.
% The implementation is in the file
% `definitions.ily`.
% Documentation (if any) should be in `README.md`.

% A1. scale all page layout variables by the same amount
\scaleVerticalSpacingPageLayout #1.5

%{

% A2. scale specific page layout variables
\scaleVerticalSpacingPageLayout
#'((all . 1)
   (system-system . 1)
   (score-system . 1)
   (markup-system . 1)
   (score-markup . 1)
   (markup-markup . 1)
   (top-system . 1)
   (top-markup . 1)
   (last-bottom . 1))

%}

% B1. scale all properties by the same amount
\scaleVerticalSpacingInSystems #1.5

%{

% B2. scale properties for specific contexts
% (or of the StaffGrouper grob, which is not a context)
\scaleVerticalSpacingInSystems
#'((all . 1)
   (staff-grouper . 1)
   (staff . 1)
   (chord-names . 1)
   (dynamics . 1)
   (figured-bass . 1)
   (lyrics . 1)
   (note-names . 1))

% B3. scale specific properties within specific contexts
% (or of the StaffGrouper grob, which is not a context)
\scaleVerticalSpacingInSystems
#'((all . 1)
   (staff-grouper-staff-staff . 1)
   (staff-grouper-staffgroup-staff . 1)
   (staff-default-staff-staff . 1) ;; same as (staff . 1)
   (chord-names-nonstaff-relatedstaff . 1)
   (chord-names-nonstaff-nonstaff . 1)
   (dynamics-nonstaff-relatedstaff . 1) ;; same as (dynamics . 1)
   (figured-bass-nonstaff-relatedstaff . 1)
   (figured-bass-nonstaff-nonstaff . 1)
   (lyrics-nonstaff-relatedstaff . 1)
   (lyrics-nonstaff-nonstaff . 1)
   (lyrics-nonstaff-unrelatedstaff . 1)
   (note-names-nonstaff-relatedstaff . 1)
   (note-names-nonstaff-nonstaff . 1)
   (note-names-nonstaff-unrelatedstaff . 1))

% B2 & B3
\scaleVerticalSpacingInSystems
#'((all . 1)
   (staff-grouper . 1)
   (staff . 1)
   (chord-names . 1)
   (dynamics . 1)
   (figured-bass . 1)
   (lyrics . 1)
   (note-names . 1)
   (staff-grouper-staff-staff . 1)
   (staff-grouper-staffgroup-staff . 1)
   (staff-default-staff-staff . 1) ;; same as (staff . 1)
   (chord-names-nonstaff-relatedstaff . 1)
   (chord-names-nonstaff-nonstaff . 1)
   (dynamics-nonstaff-relatedstaff . 1) ;; same as (dynamics . 1)
   (figured-bass-nonstaff-relatedstaff . 1)
   (figured-bass-nonstaff-nonstaff . 1)
   (lyrics-nonstaff-relatedstaff . 1)
   (lyrics-nonstaff-nonstaff . 1)
   (lyrics-nonstaff-unrelatedstaff . 1)
   (note-names-nonstaff-relatedstaff . 1)
   (note-names-nonstaff-nonstaff . 1)
   (note-names-nonstaff-unrelatedstaff . 1))

%}

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
