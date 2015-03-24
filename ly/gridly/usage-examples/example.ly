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

%%% Preliminary stuff
%%% -----------------
%%%
%%% First of all, you have to include the file that provides the
%%% public interface
\include "openlilylib"
\useLibrary gridly

%%% Grid initialization
%%% -------------------
%%%
%%% Then, this is the fundamental setup step. With \gridInit you define
%%% the dimensions of the grid: how many segments it should have and which
%%% parts it contains. In this case we are initializing a grid with
%%% four segments and three parts, namely "marks", "soprano" and "basso".
\gridInit 4 #'("marks"
               "soprano"
               "basso")

%%% You can optionally specify the grid "template", that is, the
%%% defaults for each section, that will be then applied to each
%%% part. 
%%% The \gridSetSegmentTemplate command has the following interface:
%%%
%%%    \gridSetSegmentTemplate segment-index context-modifier music
%%%
%%% where:
%%%
%%%  * `segment-index` selects the segment for which you want to set
%%%                    up the structure
%%%  * `context-modifier` is an optional parameter, that can be used
%%%                       to set the default values for the `lyrics`,
%%%                       the `opening` and the `closing` of the
%%%                       segment.
%%%                       `opening` and `closing` are snippets of music
%%%                       that can be used to start or finish spanners
%%%                       such as dynamics, beams or slurs etc.
%%%                       These can serve to ensure correct results 
%%%                       when compiling only parts of a score.
%%%  * `music` is a music expression that defines the length of the
%%%            segment. The duration specified here will be the one
%%%            used to check for correctness the durations of all the
%%%            parts of the same segment.
%%%            While it is possible to enter *any* music in this field
%%%            only its duration is taken into account.
%%%
\gridSetSegmentTemplate 1
\with {
  lyrics = \lyricmode { Fa }
}
{
  s1 |
}

\gridSetSegmentTemplate 2
\with {
  lyrics = \lyricmode { la la la! }
}
{
  s1 | s1 |
}

\gridSetSegmentTemplate 3
{
  s1 | s1 |
}


\gridSetSegmentTemplate 4
{
  s1 | s1|
}


%%% Entering music
%%% --------------
%%%
%%% Now we can start to enter the music! The interface of
%%% \gridPutMusic is the same of \gridSetSegmentTemplate, with the
%%% additional parameter `part` that specifies the part you are
%%% working on.
%%%
\gridPutMusic "soprano" 1
\with {
  lyrics = \lyricmode { Fa la }
}
\relative c' {
  e2 f |
}

\gridPutMusic "soprano" 2
\with {
  lyrics = \lyricmode { li le lo lu la! }
}
\relative c' {
  f4 a g b | c1 |
}

%%% The context modifier is optional: if you skip it, defaults for
%%% `lyrics`, `opening` and `closing` are looked up in the structure
%%% for the given section.
\gridPutMusic "basso" 1
\relative c {
  \clef "bass"
  c1 |
}

\gridPutMusic "basso" 2
\relative c {
  f2 g | c,1 |
}

\gridPutMusic "marks" 2
\relative c' {
  \mark 1
  \tempo 4=120
  s1 | s1 |
}

\gridPutMusic "marks" 3
\relative c' {
  \mark 2
  s1 | s1 |
}

\gridPutMusic "marks" 4
\relative c' {
  \mark 3
  s1 | s1 \bar "|." |
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

\gridPutMusic "soprano" 4
\with {
  lyrics = \lyricmode {li le!}
}
\relative c'' {
  g1 | c |
}

%%% Compiling single cells
%%% ----------------------
%%%
%%% It is possible to compile a single cell to a separate mini-score.
%%% This is a useful tool when working on large scores because it reduces
%%% the time needed to wait for recompilation to the very small amount
%%% that is necessary for the currently edited grid cell.
%%% 
%%% \gridCompileCell part segment
%%% will do that.
%%%
%%% The cell will be output to a file named
%%% basename-part-segment.pdf
%%% so you can even compile one or more cells along with the full score.
\gridCompileCell "soprano" 2

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
%%% This check is completely optional, but it may produce more meaningful
%%% warnings than the errors that LilyPond itself will give you when
%%% there is something wrong with the grid contents.
\gridCheck

%%% Music selection
%%% ---------------
%%%
%%% Limiting output
%%%
%%% With the function
%%%
%%%   \gridSetRange segment-selector
%%%
%%% you can limit the horizontal range of music that is processed.
%%%
%%%  - `segment-selector` is on of
%%%                       - the symbol 'all, to get all the segments
%%%                       - a pair '(start . end) to get a range of segments
%%%                       - a number, selecting a single segment
%%%                       Default is 'all
%%% It is possible to use the command multiple times and print several
%%% slices of the full score in consecutive \score blocks.
%%%
%%% Uncomment one of the following lines to see the effect:
%%%
%\gridSetRange #'(1 . 2)
%\gridSetRange 2

%%% And now, let's see the functions that are used inside a score definition
%%% to get the music out of the grid. These functions are
%%%
%%%  - \gridGetMusic part
%%%  - \gridGetLyrics part
%%%
%%% where
%%%
%%%  - `part` is the name of the part you want to get the music from

\score {

  \new StaffGroup <<
    \new Staff <<
      \new Voice { \gridGetMusic "marks" }
      \new Voice = "soprano" { \gridGetMusic "soprano" }
      \new Lyrics \lyricsto "soprano" { \gridGetLyrics "soprano" }
    >>
    \new Staff <<
      \new Voice = "basso" { \gridGetMusic "basso" }
      \new Lyrics \lyricsto "basso" { \gridGetLyrics "basso" }
    >>
  >>

  \layout{}
  \midi{}
}
