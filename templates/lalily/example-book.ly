\version "2.18.0"

% TODO: these two commands are defined in lalily, but not here!
tocPart = #tocItem
tocCollection = #tocItem

#(set-global-staff-size 16)

\include "definitions.ily"
\include "example-satb.ly"
\include "example-choir-cont.ly"

\setMusicFolder music.choral
\setTitle "Two Pieces in one book"

\addEdition book
\editionMod book 17 0/1 altatrinita.Score.A \break
\editionMod book 7 3/4 psalmXLVI.choir.cantus.Voice.A \shape #'((0 . 0)(0 . 1)(0 . 1)(0 . 0)) Slur
\editionMod book 7 3/4 psalmXLVI.choir.cantus.Voice.A \once \override Slur.color = #red

\clratree opts
% if we define an option 'keys', the bookpart will loop through all music-folders current.music.folder.<key>
\setatree opts keys #'(altatrinita psalmXLVI)
% alternatively, we can define a list with music-folders
%\setatree opts music #'((music choral altatrinita) (music choral psalmXLVI))
% create a bookpart containing all declared scores, if this file is not included
\lalilyTestBookpart #opts
