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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Export annotations to plain text file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Generate and write annotations to one or more log files
#(define (export-annotations-plaintext)
   ;
   ; TODO::
   ; - implement configurable grouping options
   ; - output to separate files
   ; - using a function to compose the stringlist for each annotation
   ;   to enable different output targets
   ;   (eventually this should be template based).
   ;   Planned target formats are (potential order implementation):
   ;   - plain text (as already is possible)
   ;   - LaTeX (-> compatible with the to-be-written critical-report package)
   ;   - JSON
   ;   - plain text with textedit links ("Frescobaldi mode")
   ;   - markdown
   ;   - HTML
   ;   - PDF(??)
   ;

   ;; process annotations, adding lines to 'messages'
   (for-each
    ;
    ; TODO:
    ; This is the part that should be factored out
    ;
    (lambda (ann)
      ;; start entry with rhythmic-location
      (append-to-output-stringlist
       (format-location ann))
      ;; add annotation type
      (append-to-output-stringlist
       (assoc-ref annotation-type-labels
         (assoc-ref ann "type")))
      ;; print properties list
      (append-to-output-stringlist
       (format-property-messages ann
         (list "type" "input-file-name" "location" "grob" "grob-location")))
      ;; add newline to annotation entry
      (append-to-output-stringlist " "))
    annotations)

   ;; write to output file
   (write-output-file "log"))

