%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of ScholarLY,                                             %
%                      =========                                              %
% a toolkit library for scholarly work with GNU LilyPond and LaTeX.           %
%                                                                             %
% ScholarLY is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU Lesser General Public License as published by %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% ScholarLY is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU Lesser General Public License    %
% along with ScholarLY.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% ScholarLY is maintained by Urs Liska, ul@openlilylib.org                    %
% Copyright Urs Liska, 2015                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Initialization of the ScholarLY library
%}

\declareLibrary ScholarLY \with {
  maintainers = "Urs Liska <ul@openlilylib.org>"
  version = "0.1.0"
  short-description = "Toolkit for scholarly editing."
  description = "
ScholarLY is intended to become a comprehensive toolkit for scholarly work with
GNU LilyPond. Currently its main content is the \annotate module, providing
commands to add annotations in the LilyPond sources. These annotations can be
printed or exported and post-processed, e.g. with critical reports in LaTeX
documents."
}

% By default coloring is turned on.
% Only for publication one will want to turn it off
\registerOption scholarly.colorize ##t

\registerOption scholarly.colors.editorial-addition #darkred

#(oll:log "Initialized ScholarLY~a" "")
