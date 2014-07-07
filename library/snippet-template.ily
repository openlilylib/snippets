% do-not-update-syntax
% (keep the above line if the snippet should *not* be updated with convert-ly)
\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Example snippet"
  snippet-short-description = \markup {
    Give a - possibly one-line - subheading for the snippet.
    This will for example be displayed in the table of content.
  }
  % provide a comma-separated list to credit multiple authors
  snippet-author = "John Doe"
  % link to the mailing list archives or a website, if applicable.
  % you can use this field more than once to add multiple references.
  snippet-source = ""
  snippet-description = \markup {
    Briefly describe what the snippet does and how to use it.
    This description will be used for the documentation
    detail pages etc.
  }
  % add one single category.
  % see ??? for the list of valid entries
  snippet-category = "none"
  % add comma-separated tags to make searching more effective.
  % preferrably use tags that already exist (see ???).
  % tag names should use lowercase and connect words using dashes.
  tags = ""
  % is this snippet ready?  See ??? for valid entries
  status = ""

  % add information about LilyPond version compatibility if available
  first-lilypond-version = ""
  last-lilypond-version = ""

% optionally add comments on issues and enhancements
  snippet-todo = ""
}

%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet:
% Anything below this line will be ignored by generated documentation.
% The snippet itself should (usually) *not* produce any output because
% it will be included in end-user files. To provide usage examples
% please create a file with the same name but an .ly extension
% in the /usage-examples directory.
%%%%%%%%%%%%%%%%%%%%%%%%

