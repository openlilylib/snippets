\version "2.18.0"

% TODO: these two commands are defined in lalily, but not here!
tocPart = #tocItem
tocCollection = #tocItem

#(set-global-staff-size 16)
\paper {
  ragged-last-bottom = ##f
}

\include "definitions.ily"
\include "example-satb.ly"
\include "example-choir-cont.ly"

\setMusicFolder music.choral
\setTitle "Two Pieces in one book"

% add some tweaks for this book
% editions added by the includes are still active! (sheet)
\addEdition book
% if a current music-folder is set, editionMod paths are relative to that path!
\editionMod book 17 0/1 altatrinita.Score.A \break % yields in music.choral.altatrinita.Score.A
\editionMod book 7 3/4 psalmXLVI.choir.cantus.Voice.A \shape #'((0 . 0)(0 . 1)(0 . 1)(0 . 0)) Slur
\editionMod book 7 3/4 psalmXLVI.choir.cantus.Voice.A \once \override Slur.color = #red

% add table of contents to book
\bookpart {
  \markuplist \table-of-contents
}

\optionsInit opts
% if we define an option 'keys', the bookpart will loop through all music-folders current.music.folder.<key>
\optionsAdd opts keys #'(altatrinita psalmXLVI)
% alternatively, we can define a list with music-folders
%\optionsAdd opts music #'((music choral altatrinita) (music choral psalmXLVI))
% create a bookpart containing all declared scores
\lalilyBookpart #opts
% lalilyTestBookpart will only act, if output-name and location match - if this is not included
