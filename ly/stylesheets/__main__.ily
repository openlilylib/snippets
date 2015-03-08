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
  Main functionality for font and stylesheet selection.
  Currently only font selection is implemented
%}

% Wrapper around choosing notation fonts.
% Place the call to it inside a \paper {} block.
\paper {
  useNotationFont =
  #(define-scheme-function (parser location options name)
     ((ly:context-mod?) string?)
     (let*
      (
        ;; Make font name case insensitive
        (use-name (string-downcase name))
        ;; create an alist with options if they are given.
        ;; if the argument is not given or no options are defined
        ;; we have an empty list.
        (options
         (if options
             (map
              (lambda (o)
                (cons (cadr o) (caddr o)))
              (ly:get-context-mods options))
             '()))
        ;; retrieve 'brace' name from options if given.
        ;; if not given we assume the same as the notation font
        (brace
         (or (assoc-ref options 'brace)
              name))
        )
      ;; if 'none' is given as brace set to default "emmentaler"
      (if (string=? "none" (assoc-ref options 'brace))
          (set! brace "emmentaler"))
      (ly:parser-define! parser 'fonts
        (set-global-fonts
         #:music use-name
         #:brace brace
         #:factor (/ staff-height pt 20)))))
}

