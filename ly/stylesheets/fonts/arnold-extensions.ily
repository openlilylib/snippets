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
  Some extensions that have been made possible through extra glyphs
  in the Arnold font. Including this file and using its commands are
  useless if Arnold has not been activated with \useNotationFont.

  Additional articulations:
  - \weakbeat
  - \strongbeat
  - \varaccent (alternative glyph for accent)

  - \altAccent activates the varaccent to be used with -> or \accent
  - \defAccent reverts to the normal accent glyph

  Additional commands (implemented as markups)
  They are structural elements in typical dodecaphonic scores.
  - \hauptstimme
  - \nebenstimme
  - \endstimme

  TODO: Consider changing these from markups to line spanners
  as they are practically always coming in pairs.
%}

%%%% Add custom articulations with non-standard glyphs from Arnold
%
% TODO:
% The properties for the articulations are completely arbitrary
% and simply copied from arbitrary existing articulations.
% TODO:
% Change glyph names for 'varaccent' when changed in font
% TODO:
% Change the glyph scaling once this is done in the font

#(append! default-script-alist
   (list
    `("weakbeat"
       . ((script-stencil . (feta . ("weakbeat" . "weakbeat")))
          (font-size . 1.5)
          ; any other properties
          (toward-stem-shift-in-column . 0.0)
          (padding . 1)
          (avoid-slur . around)
          (direction . ,UP)))
    `("strongbeat"
       . ((script-stencil . (feta . ("strongbeat" . "strongbeat")))
          ; any other properties
          (font-size . -0.5)
          (toward-stem-shift-in-column . 0.0)
          (padding . 1)
          (avoid-slur . around)
          (direction . ,UP)))
    `("varaccent"
       . ((toward-stem-shift-in-column . 0.0)
          (script-stencil . (feta . ("varsforzato" . "varsforzato")))
          ; any other properties
          (padding . 0.20)
          (side-relative-direction . ,DOWN)
          (avoid-slur . around)))
    ))

%%%% create postfix commands to use the articulations
weakbeat = #(make-articulation "weakbeat")
strongbeat = #(make-articulation "strongbeat")
varaccent = #(make-articulation "varaccent")

%%%% Switch between default and alternative accent glyphs.
%%%% This can be changed during the music.
% TODO:
% Decide about the naming (altAccent is good but inconsistent)
altAccent =
#(define-void-function (parser location)()
   (set! dashLarger varaccent))

defAccent =
#(define-void-function (parser location)()
   (set! dashLarger accent))

%%%% Add commands for typical Viennese School voicing indications
% TODO
% Check scaling once the glyphs have been updated in the font
% And check glyph name of endstimme.
scale-stimme = #-1.2

hauptstimme = \markup { \fontsize #scale-stimme \halign #1 \musicglyph #"scripts.hauptstimme" }
nebenstimme = \markup { \fontsize #scale-stimme \halign #1 \musicglyph #"scripts.nebenstimme" }
endstimme = \markup { \fontsize #scale-stimme \halign #-1.5 \musicglyph #"scripts.endvoice" }
