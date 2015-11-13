%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of Tablature,                                             %
%                      =========                                              %
% a toolkit library to enhance GNU LilyPond tablature.           %
%                                                                             %
% Tablature is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% Tablature is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with Tablature.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% Tablature is maintained by Federico Bruni, fede@inventati.org               %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Initialization of the Tablature library
%}

\declareLibrary Tablature \with {
  maintainers = "Federico Bruni <fede@inventati.org>"
  version = "0.1.0"
  short-description = "Toolkit for tablature."
  description = "
The tablature library is a collection of modules which provide features not
currently supported upstream by LilyPond.  While some of these features
might not be specific of tablatures or instruments like guitar, bass, etc.,
the name Tablature seemed to be the best comprehensive term in most cases.
It currently provides the following features: microtones in tablature;
string bending."
}
