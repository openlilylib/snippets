\version "2.18.0"
\include "general-tools/readComment/definitions.ily"

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

#(begin
  (display "If wrapped in a latex document, it can be compiled with pdflatex,")(newline)
  (display "then turned to an EPS-file and included in a markup.")(newline)
  (display "One might use markdown and pandoc to produce the PDF.")(newline)
  (newline)
  (display text)
)
