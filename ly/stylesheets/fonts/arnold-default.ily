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
  Default stylesheet for the Arnold font.
  Mainly provided by Janek Warchoł and Urs Liska to match the appearance of
  Universal Edition score of ca. 1910-1920
%}

\layout {

  \context {
    \Voice
    \override Beam.beam-thickness = 0.52
    \override Slur.thickness = 1.75
    \override Slur.line-thickness = 0.6
    \override Tie.thickness = 1.6
    \override Tie.line-thickness = 0.6
  }

  \context {
    \Lyrics
    \override LyricText.font-size = 0.3
    \override LyricText.stencil =
    #(lambda (grob)
       (ly:stencil-scale (lyric-text::print grob) 0.9 1))
  }


  \context {
    \Score
    \override DynamicText.font-size = 1.15
    \override Stem.thickness = 1.15
    \override Hairpin.thickness = 1.4
    \override Hairpin.height = 0.45
    \override InstrumentName.font-size = 1.5
    \override MetronomeMark.font-size = 1.5
    \override Glissando.thickness = 1.75
    % This is a temporary modification.
    % Squeezing the Century font makes the text fonts look
    % surprisingly similar to the original UE fonts, but it
    % is not ideal to scale the stencil as a whole becuause
    % that also squeezes notation elements.
    \override MetronomeMark.stencil =
    #(lambda (grob)
       (ly:stencil-scale (lyric-text::print grob) 0.78 1))
  }
}
