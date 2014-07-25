%%%% The stylesheet for HAYDN music notation font
%%%%
%%%% In order for this to work, this file's directory needs to 
%%%% be placed in LilyPond's path
%%%%
%%%% NOTE: If a change in the staff-size is needed, include
%%%% this file after it, like:
%%%%
%%%% #(set-global-staff-size 17)
%%%% \include "haydn.ily"
%%%%
%%%% Copyright (C) 2014 Abraham Lee (tisimst@gmail.com)

\version "2.18.2"

% The following block requires the customized "font.scm" file in order to work
\paper {
  #(define fonts
     (make-pango-font-tree 
     "haydn"  ; notation font name
     "gonville"  ; brace font name
     "Century Schoolbook L"  ; serif font name
     "Cantarell"  ; sans-serif font name
     "Inconsolata"  ; monospace font name
     (/ staff-height pt 20)))
}

% This is used to allow for local use of Haydn
% (i.e., \override Staff.NoteHead.font-family = 'haydn)
\include "font-register.ily"

