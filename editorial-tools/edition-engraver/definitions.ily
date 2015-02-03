\version "2.18.0"
\include "scheme-lib/modules.ily"

\header {
  snippet-title = "edition-engraver"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    \wordwrap {
      The edition-engraver is a tool, to add tweaks to a score without cluttering the music source with (tagged) overrides.
      Look in the README.md file for documentation.
    }
  }
  tags = "edition-engraver"
  status = "ready"
}

% import the edition-engraver from the corresponding scheme-module
#(use-modules (editorial-tools edition-engraver module))

% taken from "scm/define-context-properties.scm"
#(define (translator-property-description symbol type? description)
   (if (not (and
             (symbol? symbol)
             (procedure? type?)
             (string? description)))
       (throw 'init-format-error))

   (if (not (equal? #f (object-property symbol 'translation-doc)))
       (ly:error (_ "symbol ~S redefined" symbol)))

   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc description)
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)
% add context properties descriptions
#(translator-property-description 'edition-id list? "edition id (list)")

