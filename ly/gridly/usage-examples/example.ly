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
\loadModule "gridly"

%%% Grid initialization
%%% -------------------
%%%
%%% Then, this is the fundamental setup step. With \gridInit you define
%%% the dimensions of the grid: how many segments it should have and which
%%% parts it contains. In this case we are initializing a grid with
%%% two segments and three parts, namely "structure", "soprano" and "basso".
\gridInit 4 #'("structure"
               "soprano"
               "basso")

%%% You can optionally define "templates" for each grid column.
%%% Such a template contains defaults that will be applied to each
%%% cell in that segment where the respective fields are not overwritten.
%%% The \gridSetSegmentTemplate command has the following interface:
%%%
%%%    \gridSetSegmentTemplate segment-index context-modifier music
%%%
%%% where:
%%%
%%%  * `segment-index` selects the segment for which you want to 
%%%    define the template
%%%  * `context-modifier` is an optional parameter, that can be used
%%%    to set the default values for the 
%%%    - `lyrics`
%%%    - `opening` 
%%%    - `closing` of the segment.
%%%    `opening` and `closing` are music expressions that are ap- or
%%%    prepended to a range of music that is about to be used.
%%%    This can be used to instantiate or finalize spanners like
%%%    dynamics or slurs etc.
%%%  * `music` is a music expression that is only used to define the 
%%%    length of the segment. The duration specified here will be the one
%%%    used to check for correctness the durations of all the
%%%    parts of the same segment.
%%%

\gridSetSegmentTemplate 1
\with {
  lyrics = \lyricmode { Fa }
}
\relative c' {
  s1 |
}

\gridSetSegmentTemplate 2
\with {
  lyrics = \lyricmode { la la la! }
} 
{ s1*2 }

\gridSetSegmentTemplate 3 { s1*2 }

\gridSetSegmentTemplate 4 { s1*2 }

%%% Entering music
%%% --------------
%%%
%%% Now we can start to enter the music! The interface of
%%% \gridPutMusic is the same of \gridSetSegmentTemplate, with the
%%% additional parameter `part` that specifies the part you are
%%% working on.
%%%
%%% It makes sense to reserve one or more parts for extra information
%%% such as tempo and rehearsal marks, bars, repeats etc.
%%% It is up to the user whether this is done in one common part
%%% (such as in this example) or in individual parts.
%%% NOTE: it may be worth considering not to put this information
%%% in the music definitions but to use `edition-engraver` for this
%%% purpose, which is also part of openLilyLib
%%% TODO: Link to that once it has been migrated to the new structure.

%% Structure layer
\gridPutMusic "structure" 2 {
  \mark \default
  \tempo 4=120
  s1*2
}

\gridPutMusic "structure" 3 {
  \mark \default
  s1 
  \bar "||"
  s1
}

\gridPutMusic "structure" 4 {
  \mark \default
  s1*2 
  \bar "|."
}

%% soprano part
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

%% basso part

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
%%%  - \gridGetMusic <part>
%%%  - \gridGetLyrics <part>
%%%
%%% where `part` is the name of the part you want to get the music from.

%%% Optionally limit the output to either a pair or to a single segment.
%%% Uncomment one of the following lines to limit the compiled range:
%\gridSetRange #'(1 . 2)
%\gridSetRange 2

\score {

  \new StaffGroup <<
    \new Staff <<
      \new Voice { \gridGetMusic "structure" }
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
