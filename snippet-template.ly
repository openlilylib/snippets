\version "2.16.2" % absolutely necessary!

% use this to crop output page size to match the snippet
#(ly:set-option 'preview #t)

\header {
  snippet-title = "Example snippet"
  author = "John Doe"
  category = "stylesheets" %[should be one of a given list I think]
  long-description = "
    This snippet will make your scores look better
    by magnitudes because its creator is just _so_ ingenious."
  first-known-version = "2.16.0"
  last-known-version = "2.17.25"
  % add comma-separated tags to make searching more effective:
  tags = ""
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
