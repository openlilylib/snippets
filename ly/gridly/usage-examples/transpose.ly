\version "2.18.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%                     Gridly example: transposition
%%%                     =============================
%%%
%%% This file illustrates the usage of the transposition option in
%%% gridly. This shows how to leverage the `transposeKey` option to
%%% write the part for a clarinet in A.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% The usual includes and module loading
\include "openlilylib"
\useLibrary "gridly"

%%% Initialize the grid: three segments for a single part
\gridInit 3 #'("clarinet")

%%% Fill the grid with music. In the `with` clause you can specify the
%%% transposition key of the cell.
\gridPutMusic "clarinet" 1
\with {
  transposeKey = a,
  music = \relative c' {
    \key c \major
    c d e f |
  }
}

%%% Different cells can have different transposition keys
\gridPutMusic "clarinet" 2
\with {
  transposeKey = c,
  music = \relative c' {
    \key c \major
    c d e f |
  }
}


\gridPutMusic "clarinet" 3
\with {
  transposeKey = a,
  music = \relative c' {
    \key c \major
    c d e f |
  }
}


\score {
  
  %%% The function `gridGetMusic` will wrap each cell in a
  %%% `\transpose` block, if a transposeKey is defined.
  \gridGetMusic "clarinet"

  \layout {}
}
