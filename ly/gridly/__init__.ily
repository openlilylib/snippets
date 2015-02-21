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


%%% Initialization of the GridLY library

#(define gridly-version "0.4.0-dev")

%%% The association list holding all the music.
#(if (not (defined? 'music-grid))
     (define music-grid #f))

%%% Information that needs to be set up using \initMusicGrid
#(if (not (defined? 'music-grid-meta))
     (define music-grid-meta #f))

%%% Default segment range to the whole grid
\registerOption gridly.segment-range #'all

\void #(oll:log "Initialized GridLY version ~a" gridly-version)
