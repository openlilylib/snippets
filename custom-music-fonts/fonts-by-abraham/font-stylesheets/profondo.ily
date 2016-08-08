%%%% The stylesheet for PROFONDO music notation font (Bravura)
%%%%
%%%% In order for this to work, this file's directory needs to 
%%%% be placed in LilyPond's path
%%%%
%%%% NOTE: If a change in the staff-size is needed, include
%%%% this file after it, like:
%%%%
%%%% #(set-global-staff-size 17)
%%%% \include "profondo.ily"
%%%%
%%%% Copyright (C) 2014 Abraham Lee (tisimst@gmail.com)

\version "2.18.2"

% The following block requires the customized "font.scm" file in order to work
\paper {
  #(define fonts
     (make-pango-font-tree 
     "profondo"  ; notation font name
     "emmentaler"  ; brace font name
     "Century Schoolbook L"  ; serif font name
     "Cantarell"  ; sans-serif font name
     "Inconsolata"  ; monospace font name
     (/ staff-height pt 20)))
}

% This is used to allow for local use of Profondo
% (i.e., \override Staff.NoteHead.font-family = 'profondo)
\include "font-register.ily"

% Since Profondo is a very bold font, let's up the thickness of some lines
% (taken from the openlilylib snippets repository)
\layout {
  \override Staff.StaffSymbol.thickness = #1.2
  \override Staff.Stem.thickness = #1.6
  \override Staff.Beam.beam-thickness = #0.55
  \override Staff.Tie.thickness = #1.5
  \override Staff.Slur.thickness = #1.5
  \override Staff.PhrasingSlur.thickness = #1.5
}
