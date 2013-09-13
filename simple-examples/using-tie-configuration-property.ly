\version "2.17.25"

\header {
  title = "Using tie-configuration property"
  author = "Janek Warchoł"
  description = ""
  status = "undocumented"
  tags = "tie, ties, bezier curve"
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