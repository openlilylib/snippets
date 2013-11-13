
% TODO: 
% is this still needed (maybe someone has improved default lily's behaviour)?
% maybe there should be a bug report/feature request about this?
% add oll/snippets headers, add example.


% This function is originally copied from mark-up-title (file scm/titling.scm), 
% which is lilypond's internal function to handle the title markups. I needed 
% to replace the scopes and manually add the $defaultheader (which is internally 
% done in paper-book.cc before calling mark-up-title. Also, I don't extract the 
% markup from the header block, but use the given markup.
%
% I'm not sure if I really need the page properties in props, too... But I 
% suppose it does not hurt, either.
#(define-markup-command (markupWithHeader layout props markup) (markup?)
   "Interpret the given markup with the header fields added to the props.
This way, one can re-use the same functions (using fromproperty 
#'header:field) in the header block and as top-level markup."
(let* (
       ;; TODO: If we are inside a score, add the score's local header block, too!
       ;; Currently, I only use the global header block, stored in $defaultheader
       (scopes (list $defaultheader))
       (alists (map ly:module->alist scopes))
       (prefixed-alist
        (map (lambda (alist)
               (map (lambda (entry)
                      (cons
                       (string->symbol (string-append "header:"
                                         (symbol->string
                                          (car entry))))
                       (cdr entry)))
                 alist))
          alists))
       (props (append prefixed-alist
                props
                (layout-extract-page-properties layout))))
  (interpret-markup layout props markup)))

