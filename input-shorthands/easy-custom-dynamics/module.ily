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
  status = "ready"
}

%{
  TODOs and notes:
  ----------------

  Of course I want to eventually add this function as official lily command.

  There should be some easy way of specifying roman font, for example for
  "1st time p, 2nd f" and the like.  I suppose that

  Other things that would be nice to support:
  - text attached to hairpins
  - parenthesized and bracketed dynamics (similar to optional clef transposition)

  Later on, it would be great to have a shorthand for the function -
  for example ! (it fits the context):
  !ppppppppp
  !sfzp
  !"|molto| f"
  !p_espressivo

  A problem: when i \override DynamicText.font-name, only the font of the
  additional text should change.  Currently, the dynamic letter itself changes.
%}

#(use-modules (ice-9 regex))

dynamic =
#(define-event-function (parser location text) (markup?)
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
                          split-text)))
         #{
           #(make-dynamic-script (make-line-markup formatted))
         #})

       ;; user provided a full-blown markup, so we don't mess with it:
       #{
         #(make-dynamic-script (markup #:normal-text text))
       #}))
