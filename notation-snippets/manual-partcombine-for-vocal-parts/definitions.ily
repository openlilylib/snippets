\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Manual partcombine for vocal parts"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
    "\partcombine" function cannot handle staves with lyrics.
    Here's a workaround for combining two vocal parts.
  }
  % add comma-separated tags to make searching more effective:
  tags = "partcombine, lyrics, vocal"
  % is this snippet ready?  See meta/status-values.md
  status = "undocumented"
  %{
    WARNING! in 2.17.26 a special NullVoice context was added to LilyPond.
    Using NullVoice makes it possible to use "\partcombine" function
    with vocal music - this snippet is probably no longer needed.
  %}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

unison =
#(define-music-function (parser location music)
   (ly:music?)
   #{
     \new Devnull {
       #music
     }
   #})

divided =
#(define-music-function (parser location music)
   (ly:music?)
   #{
     \voiceTwo
     \new Voice {
       \voiceOne
       % we don't want duplicates to show:
       \override Hairpin #'stencil = ##f
       \override DynamicText #'stencil = ##f
       \override DynamicTextSpanner #'stencil = ##f
       \override TextScript #'stencil = ##f
       #music
     }
     \oneVoice
   #})



