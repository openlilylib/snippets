\version "2.16.2" % absolutely necessary!

\include "definitions.ily"

\markup \justify {
  WARNING! in 2.17.26 a special NullVoice context was added to LilyPond.
  Using NullVoice makes it possible to use "\partcombine" function
  with vocal music - this snippet is probably no longer needed.
}

% Upper voice.  Here partcombining instructions should go.

tenorI = \relative f {
  \unison {
    r2 c'4\< c
    f4. f8\! f2
    d2\mf ( c4) c
    b4.( a8 g4) g4\dim
    a2
  }
  c2\p
  c c
  r d4\mf d
  e4. g8 g2
  \divided {
    g4\f(f2 e4~
    e d2 c4~
    c b2 a4~
    a4 g2)
  }
  \unison {
    f4
    f4 ( e8 d e4) e
    g2 r
  }
}

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

% shared staff:

\new Staff = "tenory" {
  \dynamicUp
  \tupletUp
  \clef "treble_8"
  \key c \major
  \time 2/2
  <<
    \tenorI
    \tenorII
  >>
}
\addlyrics {
  me -- di -- ta -- bi -- tur
  sa -- pi -- en -- ti -- am,
  Os ju -- sti me -- di -- ta -- bi -- tur
  sa -- pi -- en -- ti -- am,
}
