\version "2.17.96"

\header {
  snippet-title = "Define variables anywhere"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "Program flow, LilyPond variables"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

parserDefine =
#(define-void-function (parser location name val)(symbol? scheme?)
    (ly:parser-define! parser name val))

%{
  To make this work in pre 2.17 versions, one might use
  string-or-symbol?
  and conditionally change name to a symbol with
  (if (string? name) (set! name (string->symbol name)))
%}
