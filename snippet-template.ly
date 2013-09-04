\version "2.16.2" % absolutely necessary!

% use this to crop output page size to match the snippet
#(ly:set-option 'preview #t)

\header {
  snippet-title = "Example snippet"
  snippet-author = "John Doe"
  snippet-description = \markup {
    This snippet will make your scores look better by magnitudes
    because its creator is just \italic so ingenious.
  }
  % add comma-separated tags to make searching more effective:
  tags = ""
  % is this snippet complete?  See meta/status-values.md
  status = ""
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%
red = {
  % color only the note head red
  \once \override NoteHead #'color = #red
}

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%
{
  d'^\markup "Color a note head red"
  \red d'( d')
}
