\version "2.16.2" % absolutely necessary!

% this will crop the output - there's no point
% in having a one-measure example placed on a
% full page.
#(ly:set-option 'preview #t)

\header {
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % The following fields are mandatory %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  snippet-title = "Example snippet"
  author = "John Doe"
  category = "stylesheets" %[should be one of a given list I think]

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % The following fields are optional %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  long-description = "
    This snippet will make your scores look better
    by magnitudes because its creator is just _so_ ingenious."
  first-known-version = "2.16.0"
  last-known-version = "2.17.25"
  tags = "tie, engraving" %[any number of tags are allowed]
  %[...?  ]
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% put the snippet here, please comment thoroughly: %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
red = {
  % color only the note head red
  \once \override NoteHead #'color = #red
}

%%%%%%%%%%%%%%%%%%
% Usage example: %
%%%%%%%%%%%%%%%%%%
{ 
  d'^\markup "Color a note head red"
  \red d'( d') 
}
