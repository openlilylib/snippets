\version "2.18.0"
\include "oll-core/package.ily"
\loadModule snippets.specific-solutions.xelatex-markup-list
%\include "definitions.ily"

text = \readComment
%{
\twocolumn{} \sloppy{}
It was a night to remember.
All of a sudden the door opened with dark noise and he, yes HE, came in.

We all were waiting for something, but we all didn't know what it was.
I was first to say "`Hello Sir"'.

-- \textit{silence} ---

It seemed like an invasion of something alien.
No one knows, if it is evil or just some kind of short intermission.

%}

\markuplist {
  \override-lines #'(tex-height . 55) % 55mm for tex output
  % Make sure the font selected in the next line is available
  \override-lines #'(font-name . "Century Schoolbook L")
  \xelatexInclude #text
  \score { \relative c'' { c4 b gis g } \addlyrics { where is your mind? } \layout {} }
}

