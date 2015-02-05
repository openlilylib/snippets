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

% By default annotations are printed
\registerOption scholarly.annotate.print ##t

%%%%%%%%%%%%%
% File export

% By default annotations are not exported
\registerOption scholarly.annotate.export-targets #'()

%%%%%%%%%%%%%%%%%
% Limiting output

% By default all annotation types are processed
\registerOption scholarly.annotate.ignored-types #'()

%%%%%%%%%%%%%%%%%%%%%%%
% Filtering annotations

% By default sort annotations in chronological order.
% For other sorting options set this option to a stringlist.
% Available sort criteria:
% - "rhythmic-location"
% - "type"
% - "author"
\registerOption scholarly.annotate.sort-criteria #'("rhythmic-location")

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Coloring annotations
%%%%%%%%%%%%%%%%%%%%%%%%%

% colors are managed as children of scholarly.annotate.colors,
% so they can be retrieved with
% \getOption scholarly.annotate.colors.<type>, e.g.
% \getOption scholarly.annotate.colors.critical-remark
% When custom annotation types are to be used a color has to be set with
% \registerOption scholarly.annotate.colors.<type> <default>
\registerOption scholarly.annotate.colors
#`((critical-remark . ,darkgreen)
   (musical-issue . ,green)
   (lilypond-issue . ,red)
   (question . ,blue)
   (todo . ,magenta))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for plain text output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For retrieving values and registering custom annotation types
% see above at the coloring section
\registerOption scholarly.annotate.export.plaintext.labels
#`((critical-remark . "Critical Remark:")
   (musical-issue . "Musical Issue:")
   (lilypond-issue . "LilyPond Issue:")
   (question . "Question:")
   (todo . "TODO:"))

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
% together with the LaTeX package, so it should not be configured on user level.
% If the functionality has to be adapted to a given project the above alist
% can be modified directly.
% An additional advantage is that with LaTeX export no special treatment of
% custom anntotation types is necessary, as these simply map to \annotation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using lilyglyphs for grob-location

% If set the grob location is formatted as a lilyglyphs command
\registerOption scholarly.annotate.export.latex.use-lilyglyphs #f

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
