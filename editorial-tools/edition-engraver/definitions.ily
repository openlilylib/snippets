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
