\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Example snippet"
  snippet-author = "John Doe"
  snippet-author-email = "john.doe@somewhere.com"
  snippet-source = "link to the mailing list archives or a website, if applicable"
  snippet-description = \markup {
    Briefly describe what the snippet does and how to use it.
  }
  % add comma-separated tags to make searching more effective:
  tags = ""
  % is this snippet ready?  See meta/status-values.md
  status = ""
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

red = {
  % color the next note red
  \once \override NoteHead #'color = #red
  \once \override Stem #'color = #red
  \once \override Flag #'color = #red
}
