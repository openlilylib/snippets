%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of ScholarLY,                                             %
%                      =========                                              %
% a toolkit library for scholarly work with GNU LilyPond and LaTeX,           %
% belonging to openLilyLib (https://github.com/openlilylib/openlilylib        %
%              -----------                                                    %
%                                                                             %
% ScholarLY is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% ScholarLY is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with ScholarLY.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% ScholarLY is maintained by Urs Liska, ul@openlilylib.org                    %
% Copyright Urs Liska, 2015                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Basic functionality to export/output annotations
%}

#(use-modules (ice-9 regex))

% Basic unescaping of plaintext message properties
% It seems that in LilyPond strings \n, \t and \" are escape sequences
% (whereas the other Guile escape sequences don't apply).
% Through this escaping we allow authors to use the combinations \n and \t
% (e.g. for LaTeX commands). \" is still escaped to a normal quotation mark
%
% Later we will have to extend this to support other input formats
% such as Markdown or HTML (or Markdown with interspersed LaTeX or HTML)
#(define basic-escape-pairs
   '(("\n" . "\\n")
     ("\t" . "\\t")))

#(define basic-escape-regexpstring "\n|\t")
#(define basic-escape-regexp (make-regexp basic-escape-regexpstring))

#(define (unescape-plaintext-message str)
   (set! str
         (regexp-substitute/global #f basic-escape-regexp str
           'pre (lambda (m)
                  (let ((ms (match:substring m)))
                    (assoc-ref basic-escape-pairs ms)))
           'post))
   str)

#(define (escape-latex-message str)
   #f)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General export helper routines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Provide a global string-list that output functions append their lines to.
% This is finally used to write to log files.
#(cond ((not (defined? 'annotate-export-stringlist))
        (define annotate-export-stringlist '())))

% Append a single string or a stringlist
% to the stringlist that is used for export
#(define (append-to-output-stringlist msg)
   (set! annotate-export-stringlist
         (append annotate-export-stringlist
           (if (list? msg)
               msg
               (list msg)))))

% Same as append-to-output-stringlist but
% operate on a given 'messages' argument.
#(define (append-to-messages messages msg)
   ;; append a single string or a string list
   (append messages
     (if (list? msg)
         msg
         (list msg))))

% As writing to a logfile uses (display)
% this is a nice shorthand (also necessary to be compatible with (write-lines)
#(define (display-line line)
   (display line)
   (newline))

% Take a string list and write it to an output using 'cmd'
% cmd could be 'display' or 'ly:message',
% basically any procedure that takes one string argument
#(define (write-lines msgs cmd)
   (for-each
    (lambda (m)
      ;; filtered properties are represented by empty strings
      ;; so we filter them out here.
      (if (not (string= m ""))
          (cmd m)))
    msgs))

% create a basename string to be used when building output file names
#(define annotation-out-basename "")
setAnnotationOutputBasename =
#(define-void-function (parser location)()
   (set! annotation-out-basename (ly:parser-output-name parser)))
\setAnnotationOutputBasename


% Take the stringlist 'annotate-export-stringlist
% and write it out to a file
#(define (write-output-file ext)
   ;
   ; TODO
   ; remove "messages" here and directly use the global object
   ;
   ; TODO
   ; Make the file name configurable and let it respect the target format
   ;
   (let* ((logfile (format "~a.annotations.~a" annotation-out-basename ext)))
     (ly:message "writing '~a' ..." logfile)
     (with-output-to-file logfile
       (lambda ()
         (write-lines annotate-export-stringlist display-line)))
     (set! annotate-export-stringlist '())))

% Format the rhythmic location of an annotation to a string
% used when printing to the console or exporting to plain text
#(define (format-location ann)
   "Return a string representation of the annotations rhythmic location"
   (let* ((props (assoc-ref ann "grob-location"))
          (measure-no (assoc-ref props "measure-no"))
          )
     (if (= 0 measure-no)
         ;; workaround for a problem that sometimes the paperColumn gets
         ;; lost along the way. In that case the location is manually
         ;; set to measure zero - which is impossible.
         (format "Sorry, rhythmic position could not be determined.\nInput location at ~a"
           (assoc-ref ann "location"))
         (format "Measure ~a, beat ~a"
           (assoc-ref props "measure-no")
           (let*
            ((beat-fraction (assoc-ref props "beat-fraction"))
             (our-beat (assoc-ref props "our-beat"))
             (beat-string (beat-string props)))
            beat-string)))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Print annotations to console
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print annotations to the console
#(define (do-print-annotations)
   (for-each
    (lambda (ann)
      (begin
       (ly:input-message (assoc-ref ann "location") "\nAnnotation:")
       (ly:message (format "    ~a" (format-location ann)))
       (write-lines
        (format-property-messages ann
          (list "type" "location" "input-file-name" "grob" "grob-location"))
        ly:message)
       (ly:message "")))
    annotations))


