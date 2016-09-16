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
\registerOption stylesheets.font.roman
\registerOption stylesheets.font.use-roman
\registerOption stylesheets.font.sans
\registerOption stylesheets.font.use-sans
\registerOption stylesheets.font.typewriter
\registerOption stylesheets.font.use-typewriter

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine installed notation fonts

% Helper function returning all notation and brace fonts from a given directory
% NOTE:
% - This only checks for the presence of an -11 font
% - In the SVG directory only SVG fonts are considered, not the WOFF ones
#(define (font-list dir ext)
   "Iterates over the files in a given directory and collects
    notation and brace fonts. Returns a pair with two sorted lists:
    - notation fonts (car)
    - brace fonts (cdr)"
   (let ((font-dir (opendir dir))
         (fonts '())
         (brace-fonts '())
         (font-rx (make-regexp (format "-11.~a" ext) regexp/icase))
         (brace-rx (make-regexp (format "-brace.~a" ext) regexp/icase))
         )
     (do ((entry (readdir font-dir)(readdir font-dir)))
       ((eof-object? entry))
       (let ((font (regexp-exec font-rx entry))
             (brace (regexp-exec brace-rx entry)))
         (if font
             (set! fonts (append fonts (list (match:prefix font)))))
         (if brace
             (set! brace-fonts (append brace-fonts (list (match:prefix brace)))))))
     (closedir font-dir)
     (cons
      (reverse (sort fonts string>?))
      (reverse (sort brace-fonts string>?)))))

% Read the OTF and SVG fonts installed in the installation of the currently executed LilyPond
% and store them in the global options
#(let*
  ((font-path (append (split-path (ly:get-option 'datadir)) '("fonts")))
   (otf-path (join-unix-path (append font-path '("otf"))))
   (otf-list (font-list otf-path "otf"))
   (svg-path (join-unix-path (append font-path '("svg"))))
   (svg-list (font-list svg-path "svg"))
   (woff-list (font-list svg-path "woff"))
   )
  #{ \registerOption global.installed-fonts.otf #(car otf-list) #}
  #{ \registerOption global.installed-fonts.otf-brace #(cdr otf-list) #}
  #{ \registerOption global.installed-fonts.svg #(car svg-list) #}
  #{ \registerOption global.installed-fonts.svg-brace #(cdr svg-list) #}
  #{ \registerOption global.installed-fonts.woff #(car woff-list) #}
  #{ \registerOption global.installed-fonts.woff-brace #(cdr woff-list) #})

% Helper function to display a set of font names
% determining the presence of a matching brace font
#(define (display-fonts fonts brace)
   (for-each
    (lambda (f)
      (let ((brace-note
             (if (not (member f brace))
                 " (no brace font)"
                 "")))
        (display (format "- ~a~a" f brace-note))
        (newline)))
    fonts))

% Display sorted lists of all notation fonts currently installed.
displayNotationFonts =
#(define-void-function (parser location)()
   (display "\nAvailable notation fonts:\nOpenType:\n")
   (display-fonts
    #{ \getOption global.installed-fonts.otf #}
    #{ \getOption global.installed-fonts.otf-brace #})
   (display "SVG:\n")
   (display-fonts
    #{ \getOption global.installed-fonts.svg #}
    #{ \getOption global.installed-fonts.svg-brace #})
   (display "WOFF:\n")
   (display-fonts
    #{ \getOption global.installed-fonts.woff #}
    #{ \getOption global.installed-fonts.woff-brace #}))


#(oll:log "Initialized Stylesheets~a" "")
