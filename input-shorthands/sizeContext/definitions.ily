\version "2.18.0"

\header {
  snippet-title = "size Staff context"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    This is a little helper command, that returns a context-modification, which scales fontSize, StaffSymbol.staff-space and StaffSymbol.thickness by s.
    This is handy to resize a Staff with one command.
  }
  % add comma-separated tags to make searching more effective:
  tags = "Staff,size,font-size"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%


sizeContext =
#(define-scheme-function (parser location s)(number?)
   #{ \with {
     fontSize = #s
     \override StaffSymbol #'staff-space = #(magstep s)
     \override StaffSymbol #'thickness = #(magstep s)
   } #})
