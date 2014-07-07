\version "2.18.0"

% show all automatic includes
includePatternVerbose = ##t

\include "definitions.ily"

\setMusicFolder musik.test.trumpet
\setTemplate lalily.instrument.brass.trumpet
\setOption #'transposition #(ly:make-pitch -1 2 -1/2) % instrument transposition e flat (b flat is default for trumpet)
\setOption #'input-concert-pitch ##t % music input in concert pitch (default)
\setOption #'output-concert-pitch ##f % music output in instrument transposition (default)

% alternatively you prepare options and set them:
%
% \optionsInit opts
% % instrument transposition e flat (b flat is default for template lalily.instrument.trumpet)
% \optionsAdd opts transposition #(ly:make-pitch -1 2 -1/2)
% % music input in concert pitch (default #t)
% \optionsAdd opts input-concert-pitch ##t
% % music output in instrument transposition (default #f)
% \optionsAdd opts output-concert-pitch ##f
% \setOptions #'() #opts
%
% or you combine setMusicFolder, setTemplate and setOptions (with a prepared a-list) to
% \setDefaultTemplate musik.test.trumpet lalily.instrument.trumpet #opts

\putMusic LY_UP.meta {
  \key f \major \time 2/4 s1 \bar "|."
}
\putMusic \relative c'' {
  bes4 a c b
}

\addEdition trumpEs
\editionMod trumpEs 2 0/4 Voice.A \once \override NoteHead.color = #red

\lalilyTest
