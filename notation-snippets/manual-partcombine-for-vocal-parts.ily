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

% lower voice (lyrics are attached to it):

tenorII = \relative f {
  r2 c'4\< c
  f4. f8\! f2
  d2\mf ( c4) c
  b4.( a8 g4) g4\dim
  a2 a2\p
  a a
  r a4\mf a
  c4. e8 e2
  a,4\f(f' g, e'
  a, d g, c~
  c b2 a4~
  a4 g2)
  f4
  f4 ( e8 d e4) e
  g2 r
}

