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

% Make general openLilyLib utilities available to any library.
% See TODO: DOC for more information
% This file is part of the openLilyLib library infrastructure
% ... TOBEDONE ...
%
% This file initializes openLilyLib

#(ly:set-option 'relative-includes #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Add openLilyLib root directory to Guile's module load path
% TODO: Check this when the Scheme lib has moved
\include "general-tools/scheme-wrapper/add-guile-path/definitions.ily"
\addGuilePath "../.."

% Make common functionality available to all openLilyLib "users"
\include "utilities/__main__.ily"

% Logging capabilities with different log levels
\include "logging.ily"

% Common option handling
\include "options.ily"

% Set default loglevel to 'warning'
% (can only be done after options have been included)
\registerOption global.loglevel #oll-loglevel-warning

% Utility to include multiple files at once
% Depends on "options.ily"
\include "utilities/include-pattern.ily"

% Set the root path of openLilyLib
% - for oll module inclusion
% - for Scheme module inclusion
% This must be called from the main openlilylib file
% because that's inside the desired root directory
setRootPath =
#(define-void-function (parser location)()
   (let* ((path (location-extract-path location)))
     #{ \registerOption global.root-path #path #}
     (if (not (member path %load-path))
         (set! %load-path `(,path ,@%load-path)))))


% Functionality to load and manage modules
\include "module-handling.ily"

% Welcome message.
% This is a default ly:message because otherwise we'd have to mess around with
% loglevels. This shouldn't be logged anyway.

#(ly:message "\nopenLilyLib: library infrastructure successfully loaded.\n\n")
