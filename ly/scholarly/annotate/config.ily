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
  This file defines most of the configuration infrastructure for \annotate
%}

%%%%%%%%%%%%%%%%%%%%%%%
% fundamental behaviour
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
% Console output

% initialize empty configuration variable
% By default annotations are printed to the console
#(cond ((not (defined? 'print-annotations))
        (define print-annotations #t)))

% Convenience function for switching annotation printing
% Specify ##t or ##f to switch on/off
printAnnotations =
#(define-scheme-function (parser location active)
   (boolean?)
   (set! print-annotations active))

%%%%%%%%%%%%%
% File export

% initialize empty configuration variable
#(cond ((not (defined? 'annotation-export-targets))
        (define annotation-export-targets '())))
#(define export-annotations #f)

% Convenience function to select output targets
% Provide a list with strings. These have to match
% an item in the export-routines alist defined in the main file.
setAnnotationExportTargets =
#(define-void-function (parser location targets)
   (stringlist?)
   (set! annotation-export-targets targets)
   (set! export-annotations #t))

%%%%%%%%%%%%%%%%%
% Limiting output

% Filter list to ignore annotation types before they are even created.
#(define ignored-annotation-types '())

% Convenience function to set the list of ignored annotation types.
% Strings passed in the stringlist argument should match existing
% annotation types, otherwise they simply don't have any effect.
ignoreAnnotationTypes =
#(define-void-function (parser location types)(stringlist?)
   (set! ignored-annotation-types types))

%%%%%%%%%%%%%%%%%%%%%%%
% Filtering annotations

% Criteria by which annotations are to be sorted.
% This is a list of keywords which can be set directly
% By default sort annotations in chronological order
#(cond ((not (defined? 'annotation-sort-criteria))
        (define annotation-sort-criteria
          '("rhythmic-location"))))
% Example: order by type and then by author
%#(set! annotation-sort-criteria '("type" "author"))

sortAnnotationsBy =
#(define-void-function (parser location criteria)
   (stringlist?)
   (set! annotation-sort-criteria criteria))

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Coloring annotations
%%%%%%%%%%%%%%%%%%%%%%%%%

% Switch coloring of annotations on/off.
% By default coloring is active
#(cond ((not (defined? 'color-annotations))
        (define color-annotations #t)))

% Convenience function for switching coloring annotations
% Specify ##t or ##f to switch on/off
colorAnnotations =
#(define-scheme-function (parser location active)
   (boolean?)
   (set! color-annotations active))

% default colors for annotations types
#(define annotation-color-defaults
   ;; Colors to be used for different types of annotation
   `(("critical-remark" . ,darkgreen)
     ("musical-issue" . ,green)
     ("lilypond-issue" . ,red)
     ("question" . ,blue)
     ("todo" . ,magenta)))

% initialize an empty alist for the annotation colors
#(cond ((not (defined? 'annotation-colors))
        (define annotation-colors '())))

% look up default annotation colors and set them
% if they aren't present yet
#(for-each
  (lambda (type)
    (if (not (assoc-ref annotation-colors (car type)))
        (set! annotation-colors
              (assoc-set! annotation-colors
                (car type) (cdr type)))))
  annotation-color-defaults)

% Convenience function to modify the colors for any annotation type.
% Expects:
% - string with annotation type (should match one of the key from
%   annotation-color-defaults above)
% - Scheme color
% TODO:
% This *should* work to seet the color of custom annotation types
% but this should be checked carefully.
% BTW what happens if a custom annotation type is used and *no*
% color is added? This should have a fallback without errors.
setAnnotationTypeColor =
#(define-void-function (parser location type color)
   (markup? color?)
   (set! annotation-colors
         (assoc-set! annotation-colors type color)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for plain text output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for each known annotation type
% Used for plain text export
#(define annotation-type-labels-defaults
   ;; default labels to be used for output in logfiles
   `(("critical-remark" . "Critical Remark:")
     ("musical-issue" . "Musical Issue:")
     ("lilypond-issue" . "LilyPond Issue:")
     ("question" . "Question:")
     ("todo" . "TODO:")))

% initialize an empty alist for the annotation type labels
#(cond ((not (defined? 'annotation-type-labels))
        (define annotation-type-labels '())))

% look up default type labels and set them
% if they aren't present yet
#(for-each
  (lambda (type)
    (if (not (assoc-ref annotation-type-labels (car type)))
        (set! annotation-type-labels
              (assoc-set! annotation-type-labels
                (car type) (cdr type)))))
  annotation-type-labels-defaults)

% Convenience function to modify the labels for any annotation type.
% Expects:
% - string with annotation type (should match one of the key from
%   annotation-type-labels above)
% - string with new label
% TODO:
% This *should* work to seet the labels of custom annotation types
% but this should be checked carefully.
% BTW what happens if a custom annotation type is used and *no*
% label is added? This should have a fallback without errors.
setAnnotationTypeLabel =
#(define-void-function (parser location type label)
   (markup? markup?)
   (set! annotation-type-labels
         (assoc-set! annotation-type-labels type label)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for LaTeX output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for each known annotation type
% Used for LaTeX export
#(define annotation-type-latex-commands
   `(("critical-remark" . "\\criticalRemark")
     ("musical-issue" . "\\musicalIssue")
     ("lilypond-issue" . "\\lilypondIssue")
     ("question" . "\\annotateQuestion")
     ("todo" . "\\annotateTodo")))

% There is no implementation of convenience commands because this should
% not actually be necessary. The LaTeX implementation is designed to work
% together with the LaTeX package, so it should note be configured on user level.
% If the functionality has to be adapted to a given project the above alist
% can be modified directly.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using lilyglyphs for grob-location

% Initialize configuration variable
#(cond ((not (defined? 'format-beat-string-with-lilyglyphs))
        (define format-beat-string-with-lilyglyphs #f)))

% Convenience function to set the behaviour
useLilyglyphsForBeatString =
#(define-void-function (parser location active)
   (boolean?)
   (set! format-beat-string-with-lilyglyphs active))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for property fields
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for annotation properties
% Used for console printing and plain text  export
#(define annotation-property-labels-defaults
   `(("message" . "Message")
     ("author" . "Author(s)")
     ("context-id" . "Context")
     ("source" . "Affected Source")
     ("voice-name" . "Voice")
     ("segment-name" . "File")
     ("grob-type" . "Affected Item")))

% initialize an empty alist for the annotation type labels
#(cond ((not (defined? 'annotation-property-labels))
        (define annotation-property-labels '())))

% look up default type labels and set them
% if they aren't present yet
#(for-each
  (lambda (type)
    (if (not (assoc-ref annotation-property-labels (car type)))
        (set! annotation-property-labels
              (assoc-set! annotation-property-labels
                (car type) (cdr type)))))
  annotation-property-labels-defaults)

% Convenience function to modify the labels for any annotation type.
% Expects:
% - string with property type (should match one of the key from
%   annotation-property-label-defaults above)
% - string with new label
% For custom properties that don't have a defined label
% the plain property name is used.
setAnnotationPropertyLabel =
#(define-void-function (parser location prop label)
   (markup? markup?)
   (set! annotation-property-labels
         (assoc-set! annotation-property-labels prop label)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for voice/context names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \annotate tries to determine a proper lable for the
% musical context. It does so by in a three-step
% fall-through solution.
%
% 1) If the annotation defines a 'context' property
%    this value is used.
% 2) If the Staff-level context where the annotation
%    is defined is explicitly named this name is used
% 3) Without any explicit naming the directory name of
%    the file in which the annotation is entered is used.
%
% For pretty-printing or localizing \annotate
% supports re-labelling of these context names.

% Initialize empty alist.
#(cond ((not (defined? 'annotation-context-labels))
        (define annotation-context-labels '())))

% Return the label for the given context/part name
% or the name of the context itself if no label is defined
#(define (annotation-context-label context)
   (let*
    ((ctx (ly:context-id context)))
    (or
     (assoc-ref annotation-context-labels ctx)
     ctx)))

% Convenience function to add labels for context names.
% Supply an alist with one pair for each instrument,
% e.g. #'(("piano" . "Klavier"))
addAnnotationContextLabels =
#(define-void-function (parser location labels)
   (list?)
   (for-each
    (lambda (l)
      (set! annotation-context-labels
            (assoc-set! annotation-context-labels
              (car l) (cdr l))))
    labels))
