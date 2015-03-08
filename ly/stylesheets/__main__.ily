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

% Use a notation font with or without options.
% To be called as a toplevel command.
% Arguments:
% - options: ly:context-mod? ( a "\with {}" clause)
%   - brace: define brace font
%            - "none": default to Emmentaler
%            - omitted: use the same as the font name
%   - style: load a style sheet for the font
%            - omitted: load the "-default" stylesheet
%                       (has to be provided by the library)
% - name: Font name
%
% All arguments are case insensitive, so "Emmentaler" is
% equivalent to "emmentaler".
% Note: If the names do not contain characters beyond alphabetical
% and a hyphen (but no numbers), the quotation marks can be omitted,
% so
%    \useNotationFont Beethoven
% is valid while with
%    \useNotationFont "Gutenberg1939"
% the quotation marks are needed.


#(define (make-style-file use-name style)
   "Construct file name for included style sheet.
    Factored out because needed several times."
   (string-append
    #{ \getOption global.root-path #}
    "/stylesheets/fonts/"
    use-name
    "-"
    (string-downcase style)
    ".ily"))

useNotationFont =
#(define-void-function (parser location options name)
   ((ly:context-mod?) string?)
   (let*
    (
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
      (use-brace (string-downcase brace))
      ;; retrieve 'style' option with "default" default ...
      (style
       (or (assoc-ref options 'style)
           "default"))
      ;; ... and produce include filename from it
      (style-file (make-style-file use-name style))
      )

    ;; Post-process options
    ;;
    ;; if 'none' is given as brace set to default "emmentaler"
    (if (string=? "none" (assoc-ref options 'brace))
        (set! brace "Emmentaler"))

    ;; if a non-existent stylesheet is requested
    ;; issue a warning and reset to -default
    (if (not 
         (or (string=? "none" style)
             (file-exists? style-file)))
        (begin
         (oll:warn location
           (format "Requested stylesheet \"~a\" doesn't exist for font \"~a\"" style name))
         (set! style-file (make-style-file use-name style))))

    ;; store options, these are used from the included load-font file
    #{ \setOption stylesheets.font.name #name #}
    #{ \setOption stylesheets.font.use-name #use-name #}
    #{ \setOption stylesheets.font.brace #brace #}
    #{ \setOption stylesheets.font.use-brace #use-brace #}

    ;; load font through an included file.
    ;; this is necessary so that file can set its own
    ;; \paper {} block.
    ;
    ; TODO:
    ; Find a way to pull that functionality in here.
    ; The problem seems to be that (even when wrapping the
    ; definition of 'fonts in a ly:parser-define call the
    ; properties of the \paper block (e.g. staff-height) are
    ; not available.
    ; I think one has to somehow access the current paper block
    ; through Scheme (I suspect there are options in the paper
    ; related ly: functions but I didn't succeed to find a solution).
    (ly:parser-include-string parser
      (ly:gulp-file
       (string-append
        #{ \getOption global.root-path #}
        "/stylesheets/load-font")))
    (oll:log location
      (format "Font \"~a\" loaded successfully" name))

    ;; include the determined style file for the font
    ;; if not "none".
    (ly:message style)
    (if (not (string=? "none" style))
        (ly:parser-include-string parser
          (ly:gulp-file style-file)))
    (oll:log location (format "Associated \"~a\" stylesheet loaded successfully" style))
    ))


%%%% ARNOLD extensions
% The Arnold font provides a number of extra glyphs
% This command loads a separate style sheet defining commands
% and articulations to make use of these extra glyphs.
useArnoldExtensions =
#(define-void-function (parser location)()
   (ly:parser-include-string parser
     (ly:gulp-file
      (string-append
       #{ \getOption global.root-path #}
       "/stylesheets/fonts/arnold-extensions.ily"))))
