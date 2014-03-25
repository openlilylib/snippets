\version "2.18.0"

% show all included template-files
includePatternVerbose = ##t

\include "definitions.ily"

\clralist opts
\addalist opts transposition #(ly:make-pitch -1 2 -1/2) % instrument transposition e flat (b flat is default for trumpet)
\addalist opts input-concert-pitch ##t % music input in concert pitch (default)
\addalist opts output-concert-pitch ##f % music output in instrument transposition (default)

\setDefaultTemplate musik.test.trumpet lalily.instrument.trumpet #opts
\putMusic LY_UP.meta {
  \key f \major \time 2/4 s1 \bar "|."
}
\putMusic \relative c'' {
  bes4 a c b
}

\addEdition trumpEs
\editionMod trumpEs 2 0/4 Voice.A \once \override NoteHead.color = #red

\lalilyTest
