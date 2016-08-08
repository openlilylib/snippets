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

#(cond ((not (defined? 'edition-engraver-loaded))
        (define edition-engraver-loaded #f)))

% import the edition-engraver from the corresponding scheme-module
% but only if it hasn't already been loaded
#(if (not edition-engraver-loaded)
     (begin
      (use-modules (editorial-tools edition-engraver module))
      (set! edition-engraver-loaded #t)))
