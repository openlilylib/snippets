\version "2.16.2"

\header {
  snippet-title = "Easy custom dynamics"
  snippet-author = "Janek Warchoł"
  snippet-source = ""
  snippet-description = \markup {
    Simple, user-friendly way to define custom dynamics
    with minimal fuss.  Accepts markups and plain text definitions.
  }
  snippet-dedication = "dedicated to Marc and Tine Hohl :-)"
  % add comma-separated tags to make searching more effective:
  tags = "dynamics"
  % is this snippet ready?  See meta/status-values.md
  status = ""
}


%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(use-modules (ice-9 regex))

dynamic =
#(define-music-function (parser location text) (markup?)
   (if (string? text)
       (let* ((underscores-replaced
               (string-map
                (lambda (x) (if (eq? x #\_) #\space x))
                text))
              (split-text (string-split underscores-replaced #\space))
              (formatted (map
                          (lambda (word)
                            (if (string-match "^[mrzfps]*$" word)
                                (markup #:dynamic word)
                                (markup #:normal-text #:italic word)))
                          split-text))
              )
         #{
           #(make-dynamic-script (make-line-markup formatted))
         #})

       ;; user provided a full-blown markup, so we don't mess with it:
       #{
         #(make-dynamic-script (markup #:normal-text text))
       #}))

%%%%%%%%%%%%%%%%%%%
% usage examples: %
%%%%%%%%%%%%%%%%%%%

{
  c'1 -\dynamic sfffzppppp
}
{
  c' -\dynamic "molto f ekspressivvo"
}
{
  c' -\dynamic fff_I_can_use_underscores
}
{
  c' -\dynamic \markup { lolish \huge \dynamic pp \italic ekspress, \caps "markups ftw!" }
}