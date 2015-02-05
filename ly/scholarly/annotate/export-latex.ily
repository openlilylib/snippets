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
  Output annotations as LaTeX code
%}

#(use-modules (ice-9 regex))

% A list with characters that should be escaped when
% exporting to LaTeX. This works together with the below regexp
% and is used in the normalization function.
#(define latex-escape-pairs
   '(("\\" . "\\textbackslash ")
     ("&" . "\\&")
     ("{" . "\\{")
     ("}" . "\\}")
     ("[" . "{[}")
     ("]" . "{]}")
     ("#" . "\\#")
     ("\n\n" . "\\\\\n")))

latex-escape-regexpstring = "&|\\\\|\{|\}|\[|\]|#|\@.*\@"
latex-escape-regexp = #(make-regexp latex-escape-regexpstring)

% LilyPond strings can contain stuff that is not accepted in LaTeX files,
% so we have to preprocess them).
% Verbatim LaTeX code can be inserted by enclosing it with at-symbols
#(define (escape-string-latex str)
   ;; Escape invalid LaTeX characters
   (set! str
         (regexp-substitute/global #f latex-escape-regexp str
           'pre (lambda (m)
                  (let ((ms (match:substring m)))
                    (if (string= "@" (substring ms 0 1))
                        ;; return verbatim LaTeX code (without the @-s)
                        (substring ms 1 (- (string-length ms) 1))
                        ;; return escaped character(s)
                        (assoc-ref latex-escape-pairs ms))))
           'post))
   str)

#(define (indent-multiline-latex-string str)
   ;; make nice indentation
   (set! str
         (regexp-substitute/global #f "\n"
           (regexp-substitute/global #f "\n +" str
             'pre "\n" 'post)
           'pre "\n     " 'post))
   ;; "return" normalized string
   str)

% Return a string list for the "remaining" properties,
% formatted as a list of key=value arguments
#(define (format-latex-remaining-properties type props loc-props)
   (let ((cmd
          (or (assoc-ref annotation-type-latex-commands type)
              "\\annotation"))
         (props
          (map
           (lambda (p)
             (cons (car p)
               (if (ly:music? (cdr p))
                   (format-ly-music (cdr p))
                   (cdr p))))
           props))
         (result '()))
     ;; Start with LaTeX command
     (set! result
           (append-to-messages result
             (format "~a" cmd)))
     ;; First line of optional argument with opening bracket
     (set! result
           (append-to-messages result
             (format "   [~a={~a}," (car (first props)) (cdr (first props)))))
     (set! props (cdr props))
     ;; write all remaining properties as key=value pair
     (for-each
      (lambda (p)
        (set! result
              (append-to-messages result
                (format "    ~a={~a}," (car p) (cdr p)))))
      props)
     ;; properly close last entry
     (list-set! result (- (length result) 1)
       (format "~a]"
         (string-copy
          (last result)
          0
          (- (string-length (last result)) 1))))
     result))

% Lookup list for lilyglyphs representations of rhythmic values
#(define lilyglyphs-rhythmic-values
   '((1 . "\\wholeNote")
     (1/2 . "\\halfNote")
     (1/4 . "\\crotchet")
     (1/8 . "\\quaver")
     (1/16 . "\\semiquaver")
     (1/32 . "\\semidemiquaver")))

% Lookup a lilyglyphs representation for a rhythmic fraction
% or return a 'NA' string and issue a warning
% This is probably mostly an issue with the display of beat fractions
#(define (lilyglyphs-lookup frac)
   (or (assoc-ref lilyglyphs-rhythmic-values frac)
       ; Currently we don't provide any intelligent handling of
       ; values that are not in the above list.
       (begin
        (ly:warning (format "Did not find a lilyglyphs representation for ~a" frac))
        "NA")))

% If requested format the measure position using lilyglyphs commands
#(define (lilyglyphs-beat-string loc-props)
   (let*
    ((meter (assoc-ref loc-props "meter"))
     (beat-length (/ 1 (cdr meter)))
     (beat-lily (lilyglyphs-lookup beat-length))
     (our-beat (assoc-ref loc-props "our-beat"))
     (beat-string (format "~a.\\,~a" our-beat beat-lily))
     (beat-fraction (assoc-ref loc-props "beat-fraction"))
     (beat-part (assoc-ref loc-props "beat-part")))

    (if (= 0 beat-fraction)
        beat-string
        (format "~a,\\,~a" beat-string
          (let*
           ((part-numerator (+ 1 (ly:moment-main-numerator beat-part)))
            (sub-beat-length (/ 1 (ly:moment-main-denominator beat-part))))
           (format "~a. ~a" part-numerator
             (lilyglyphs-lookup sub-beat-length)))))))

% Generate and write annotations to a LaTeX file
#(define (export-annotations-latex)
   ;
   ; TODO::
   ; - implement configurable grouping options
   ; - output to separate files
   ; - using a function to compose the stringlist for each annotation
   ;   to enable different output targets
   ;   (eventually this should be template based).
   ;   Planned target formats are (potential order implementation):
   ;   - JSON
   ;   - plain text with textedit links ("Frescobaldi mode")
   ;   - markdown
   ;   - HTML
   ;   - PDF(??)
   ;

   ;; process annotations, adding lines to 'annotate-export-stringlist'
   (for-each
    ;
    ; TODO:
    ; This is the part that should be factored out
    ;
    (lambda (ann)
      (let*
       ((loc-props (assoc-ref ann "grob-location"))
        (rem-props (list-copy ann)))

       ;; Create a list rem-props with "remaining properties"
       ;; that are not used explicitly.
       (for-each
        (lambda (p)
          (set! rem-props (assoc-remove! rem-props p)))
        (list "type" "grob-type" "context-id" "input-file-name"
          "message" "location" "grob" "grob-location"))

       ;; If there are remaining properties
       ;; output them to a key-value list as an optional argument
       ;; otherwise write a simple command
       (if (> (length rem-props) 0)
           (append-to-output-stringlist
            (format-latex-remaining-properties
             (assoc-ref ann "type") rem-props loc-props))
           ;; start entry with LaTeX command and rhythmic location
           (append-to-output-stringlist
            (format "~a"
              ;                     (format "~a{~a}{~a}"
              (assoc-ref annotation-type-latex-commands
                (assoc-ref ann "type")))))
       ;; output location arguments
       (append-to-output-stringlist
        (format "    {~a}{~a}"
          (assoc-ref loc-props "measure-no")
          (if #{ \getOption scholarly.annotate.export.latex.use-lilyglyphs #}
              (lilyglyphs-beat-string loc-props)
              (beat-string loc-props))))

       ;; Affected context
       (append-to-output-stringlist
        (format "    {~a}"
          (assoc-ref ann "context-id")))
       ;; Affected grob type
       (append-to-output-stringlist
        (format "    {~a}"
          (assoc-ref ann "grob-type")))

       ;; The actual message
       ;; Invalid characters are escaped except for intended
       ;; verbatim LaTeX code
       (append-to-output-stringlist
        (format "    {~a}"
          (indent-multiline-latex-string
           (escape-string-latex
            (assoc-ref ann "message")))))

       ;; For a custom annotation we have to append
       ;; the type as 7th argument
       (let ((type (assoc-ref annotation-type-latex-commands
                     (assoc-ref ann "type"))))
         (if (not type)
             (append-to-output-stringlist
              (format "    {~a}" (assoc-ref ann "type")))))
       ;; add newline to annotation entry
       (append-to-output-stringlist " ")))
    annotations)

   ;; write to output file
   (write-output-file "inp"))

