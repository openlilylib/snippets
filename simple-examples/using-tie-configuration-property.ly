\version "2.17.25"

\header {
  snippet-title = "Using tie-configuration property"
  snippet-author = "Janek Warchoł"
  snippet-description = ""
  tags = "tie, ties, bezier curve"
  status = "undocumented"
}

% put the snippet here:

{
  b'1 ~
  \override TieColumn #'tie-configuration = #'((2.0 . -1))
  b'1
  <b' d''>1 ~
  \override TieColumn #'tie-configuration = #'((2.0 . -1) (-1.0 . -1))
  q1
}