%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of Stylesheets,                                           %
%                      ===========                                            %
% a library to manage and use style sheets and alternative fonts with         %
% the GNU LilyPond engraving software,                                        %
% belonging to openLilyLib (https://github.com/openlilylib/openlilylib        %
%              -----------                                                    %
%                                                                             %
% Stylesheets is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% Stylesheets is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with Stylesheets.  If not, see <http://www.gnu.org/licenses/>.        %
%                                                                             %
% Stylesheets is maintained by                                                %
% - Urs Liska, ul@openlilylib.org                                             %
% - Kieren MacMillan, kieren_macmillan@sympatico.ca                           %
% - Abraham Lee, tisimst.lilypond@gmail.com                                   %
% Copyright Urs Liska / Kieren MacMillan, Abraham Lee, 2015                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Initialization of the Stylesheets library
%}

\declareLibrary "Stylesheets" \with {
  maintainers = #'("Urs Liska <ul@openlilylib.org>"
                   "Abraham Lee <tisimst.lilypond@gmail.com>"
                   "Kieren MacMillan <kieren_macmillan@sympatico.ca>")
  version = "0.1.0"
  short-description = "Managing fonts and stylesheets with GNU LilyPond"
  description = "Longer description, used as an introduction to the library."
  % The following option is "unreal" and only used to demonstrate "known options"
  lilypond-min-version = "2.18.2"
}


% internal options for use in the font loading mechanism
\registerOption stylesheets.font.name Emmentaler
\registerOption stylesheets.font.use-name Emmentaler
\registerOption stylesheets.font.brace Emmentaler
\registerOption stylesheets.font.use-brace Emmentaler

#(oll:log "Initialized Stylesheets~a" "")
