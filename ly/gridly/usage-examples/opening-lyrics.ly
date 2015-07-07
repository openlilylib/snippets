\version "2.18.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%                Gridly example: opening lyrics
%%%                ==============================
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% The usual includes and module loading
\include "openlilylib"
\loadModule "gridly"

%%% Initialize the grid: three segments for a single part
\gridInit 2 #'("voice")

%%% Fill the grid with music. In the `with` clause you can specify the
%%% opening, the closing and their lyrics
\gridPutMusic "voice" 1
\with {
  closing = \relative c' { c }
  lyrics = \lyricmode { This is a test }
}
\relative c' {
  \key c \major
  c e g c, ~ |
}

\gridPutMusic "voice" 2
\with {
  opening = \relative c' { \partial 4 c4 ~ }
  opening-lyrics = \lyricmode { test }
  lyrics = \lyricmode { a simple test! }
}
\relative c' {
  c d b c |
}

\gridCompileCell "voice" 1
\gridCompileCell "voice" 2

\gridSetRange #'all

\score {
  <<
    \new Voice = "test" { \gridGetMusic "voice" }
    \new Lyrics \lyricsto "test" { \gridGetLyrics "voice" }
  >>

  \layout {}
}
