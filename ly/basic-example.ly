%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib/openlilylib                                 %
%              -----------                                                    %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2015                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.18.0"

%{
  Basic example demonstrating the general use of openLilyLib
%}

% Load the basic functionality
\include "openlilylib"

% Setting loglevel to 'log', otherwise one wouldn't see some instructional messages
\setOption global.loglevel #oll-loglevel-log

% Load the example library
% Notice (on the console) that actually example/__main__.ily is loaded
\loadModule "example"

#(ly:message "\nTry to load a module that is already loaded:")
\loadModule "example/load-test.ily"

#(ly:message "\nTry to load a non-existent module:")
\loadModule "example/this/is/not/a/module.ily"


#(ly:message "Use a command defined in the loaded modules")
\hello

#(ly:message "\nOverwrite one option, keep default of another, try to set a non-existent option.")
\setOption example.common.thickness 0.8
\setOption example.common.thin-thickness 0.5
#(ly:message (format "Default value of example.common.thick-thickness: ~a\n"
               #{ \getOption example.common.thick-thickness #}))

\displayOptions