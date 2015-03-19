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
  Some extensions supporting engraving with LilyJAZZ

%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start with a repeat Barline
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showStartRepeatBar = {
  \once \override Score.BreakAlignment.break-align-orders =
        #(make-vector 3 '(instrument-name
                          left-edge
                          ambitus
                          breathing-sign
                          clef
                          key-signature
                          time-signature
                          staff-bar
                          custos))
      \once \override Staff.TimeSignature.space-alist =
        #'((first-note . (fixed-space . 2.0))
           (right-edge . (extra-space . 0.5))
           ;; free up some space between time signature
           ;; and repeat bar line
           (staff-bar . (extra-space . 1)))
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating jazz-style repeats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%=> http://lsr.di.unimi.it/LSR/Item?id=753
#(define (white-under grob) (grob-interpret-markup grob
  (markup #:vcenter #:whiteout #:pad-x 1 (ly:grob-property grob 'text))))

inlineMMR = {
  \once \override MultiMeasureRest.layer = #-2
  \once \override MultiMeasureRestNumber.layer = #-1
  \once \override MultiMeasureRestNumber.Y-offset = #0
  \once \override MultiMeasureRestNumber.stencil = #white-under
  \once \override MultiMeasureRest.rotation = #'(2 0 0)
}


