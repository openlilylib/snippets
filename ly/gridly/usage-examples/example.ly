\version "2.18.2"

%% gridly - simple segmented grid for LilyPond
%% Copyright (C) 2015 - Matteo Ceccarello
%% 
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%                        Gridly example
%%%                        ==============
%%%
%%% This file illustrate the basic usage of Gridly with a single file.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(ly:set-option 'relative-includes #t)

%%% Preliminary stuff
%%% -----------------
%%%
%%% First of all, you have to include the file that provides the
%%% public interface
\include "openlilylib"
\loadModule "gridly"

%%% Grid initialization
%%% -------------------
%%%
%%% Then, this is the fundamental setup step. With \gridInit you define
%%% the dimensions of the grid: how many segments it should have and which
%%% parts it contains. In this case we are initializing a grid with
%%% two segments and two parts, namely "soprano" and "basso".
\gridInit #4 #'("soprano"
                "basso")

%%% You can optionally specify the grid "structure", that is, the
%%% defaults for each section, that will be then applied to each
%%% part. The \gridSetStructure command has the following interface:
%%%
%%%    \gridSetStructure segment-index context-modifier music
%%%
%%% where:
%%%
%%%  * `segment-index` selects the segment for which you want to set
%%%                    up the structure
%%%  * `context-modifier` is an optional parameter, that can be used
%%%                       to set the default values for the `lyrics`,
%%%                       the `opening` and the `closing` of the
%%%                       segment.
%%%  * `music` is a music expression that defines the length of the
%%%            segment. The duration specified here will be the one
%%%            used to check for correctness the durations of all the
%%%            parts of the same segment.
%%%
\gridSetStructure #1
\with {
  lyrics = \lyricmode { Fa }
}
\relative c' {
  s1 |
}

%%% The `music` parameter may also include other commands, such as
%%% marks, tempo changes, bars, etc. You then have the possibility to
%%% include the structure above the staves, thus including the effects
%%% of these commands in the score.
\gridSetStructure #2
\with {
  lyrics = \lyricmode { la la la! }
}
\relative c' {
  \mark #1
  \tempo 4=120
  s1 | s1 |
}

\gridSetStructure #3
\relative c' {
  \mark #2
  
  s1 | s1 |
}

\gridSetStructure #4
\relative c' {
  \mark #3
  
  s1 | s1 \bar "|." |
}

%%% Entering music
%%% --------------
%%%
%%% Now we can start to enter the music! The interface of
%%% \gridPutMusic is the same of \gridSetStructure, with the
%%% additional parameter `part` that specifies the part you are
%%% working on.
%%%
\gridPutMusic "soprano" #1
\with {
  lyrics = \lyricmode { Fa la }
}
\relative c' {
  e2 f |
}

\gridPutMusic "soprano" #2
\with {
  lyrics = \lyricmode { li le lo lu la! }
}
\relative c' {
  f4 a g b | c1 |
}

%%% The context modifier is optional: if you skip it, defaults for
%%% `lyrics`, `opening` and `closing` are looked up in the structure
%%% for the given section.
\gridPutMusic "basso" #1
\relative c {
  \clef "bass"
  c1 |
}

\gridPutMusic "basso" #2
\relative c {
  f2 g | c,1 |
}

%%% Missing cells
%%% -------------
%%% 
%%% As for missing cells, if there is structure defined for them (as
%%% in this example), then they will be replaced in the output by
%%% dummy cells, filled with skips. If the structure is not defined,
%%% then you will get an error.
%%%
%%% Here, for section 3 we are not defining any music. For section 4,
%%% we define only the soprano, so that you can see the effects of
%%% these undefined cells in the output (and in the printed grid as
%%% well)

\gridPutMusic "soprano" #4
\with {
  lyrics = \lyricmode {li le!}
}
\relative c'' {
  g1 | c |
}

%%% Display the grid
%%% ----------------
%%%
%%% With the following command you can print the current grid state,
%%% with `o` symbols marking entered segments and `-` symbols denoting
%%% missing segments.
\gridDisplay

%%% Checking the grid contents
%%% --------------------------
%%%
%%% The following command checks that, for each segment, each part of
%%% that segment has the same duration as the one specified in the
%%% structure for that segment. If not, it issues a warning on the
%%% console. If there is no structure specified for that segment
%%% (remember, structure specification is optional), then the function
%%% checks that all the parts have the same duration for that segment.
\gridCheck

%%% Music selection
%%% ---------------
%%%
%%% And now, let's see the functions that are used to get the music
%%% out of the grid. These functions are
%%%
%%%  - \gridGetStructure segment-selector
%%%  - \gridGetMusic part segment-selector
%%%  - \gridGetLyrics part segment-selector
%%%
%%% where
%%%
%%%  - `part` is the name of the part you want to get the music from
%%%  - `segment-selector` is either the symbol 'all, to get all the
%%%                       segments, or a tuple '(start . end) to get
%%%                       the segments from start up to end,
%%%                       inclusive.

%%% For convenience, let's store the selector in a variable. With 'all
%%% we are selecting all the segments. If instead we wanted only a
%%% range of segments, say from 2 to 5, we could have used the tuple
%%% '(2 . 5). To get a single segment, for instance 3, use a single
%%% integer, 3.
segments = #'all

\score {
  
  \new StaffGroup <<
    \new Staff <<
      \new Voice { \gridGetStructure \segments }
      \new Voice = "soprano" { \gridGetMusic "soprano" \segments }
      \new Lyrics \lyricsto "soprano" { \gridGetLyrics "soprano" \segments }
    >>
    \new Staff <<
      \new Voice = "basso" { \gridGetMusic "basso" \segments }
      \new Lyrics \lyricsto "basso" { \gridGetLyrics "basso" \segments }
    >>
  >>

  \layout{}
  \midi{}
}
