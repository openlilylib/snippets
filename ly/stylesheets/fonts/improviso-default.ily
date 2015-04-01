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
  Default stylesheet for the IMPROVISO notation font.
  Copyright (C) 2014-2015
  - Abraham Lee (tisimst.lilypond@gmail.com),
  - Urs Liska (ul@openlilylib.org)
%}

\version "2.18.2"

\layout {
  \context {
    \StaffGroup
    \override SystemStartBracket.thickness = #0.25
  }
  \context {
    \Score
    \override Beam.beam-thickness = #0.5
    \override Beam.length-fraction = #1.0
    \override StaffSymbol.thickness = 1.4
    \override Stem.thickness = #1.75
    \override Hairpin.thickness = #2
    \override PianoPedalBracket.thickness = #2
    \override Tie.thickness = #2
    \override Slur.thickness = #2
    \override PhrasingSlur.thickness = #2
    \override MultiMeasureRestNumber.font-size = #2
    \override LyricHyphen.thickness = #3
    \override LyricExtender.thickness = #3
    \override Glissando.thickness = 1.75
    \override VoltaBracket.thickness = #2
  }
}
