\version "2.18.0"
\include "templates/lalily/definitions.ily"

% setup options ...
\optionsInit opts
\optionsAdd opts part.choir.template \Path lalily.vocal.group
\optionsAdd opts part.choir.staff-mods \with { midiInstrument = "choir aahs" }
\optionsAdd opts part.choir.staffs.cantus.staff-mods \with { instrumentName = "Cantus" }
\optionsAdd opts part.choir.staffs.cantus.clef "G"
\optionsAdd opts part.choir.staffs.tenor.staff-mods \with { instrumentName = "Tenor" }
\optionsAdd opts part.choir.staffs.tenor.clef "G_8"
\optionsAdd opts part.choir.staffs.bass.staff-mods \with { instrumentName = "Bass" }
\optionsAdd opts part.choir.staffs.bass.clef "bass"
\optionsAdd opts part.continuo.template \Path lalily.instrument
\optionsAdd opts part.continuo.name "continuo"
\optionsAdd opts part.continuo.clef "bass"
\optionsAdd opts part.continuo.staff-mods \with {
         instrumentName = \markup { \override #'(baseline-skip . 2) \right-column { Basso Continuo } }
         midiInstrument = "church organ"
}

% set current music folder
\setMusicFolder music.choral.psalmXLVI
% bind template to current music-folder
\setTemplate lalily.group
% bind options to current music folder
\setOptions #'() #opts
% bind title to current music-folder
% title is also stored to options, so we set it after we assigned the other options
\setTitle "Psalm 46. Ein feste Burg ist unser Gott"

% meta part for music
\putMusic meta {
  \time 4/2 \key c \major
  s\breve*61 \once \override Staff.BarLine.transparent = ##f \bar "|."
}

% choir: cantus, tenor and bass with music and lyrics
\putMusic choir.cantus.music \relative c'' {
  r4 c c c g2 a4 c2( b4 a) a g1 | r1 r4 d f2.( e4 d) d c1 |
  R\breve r1 r4 c' c c g2 a4 c2( b4 a) a g1 r2 r4 g |
  g g d2 e4 g2( f4 | e) e d2 r4 c' b2 | a g r4 a f2. e4 d2 c4 c' c c |
  g2 r4 g g g d2 | r4 d' d d a1 | R\breve | r1 r4 c b a |
  g2 r4 c b2 a | g a4 f2( e4 d2) | c1 r1 | r1 r2 c'2 |
  b1 a | g a2 f1( e2 d1) | c r | r r4 c g' a8 b |
  c1 r | r r4 d, g a8 b | c2 r4 c, e4.( f8 g2) | r4 d f4.( g8 a1) |
  r4 c, e4.( f8 g2) a1( g) fis2 | g c, g'1 | a2 b c r4 b | c b a g r b c b |
  a g r e f e d c | r c' d2 c r4 b | c1. b2 | a1 g | r1 r4 a a g |
  a f e2 r2 r4 a | a g a f e2 r | r4 d' d cis d b a2 |
  r2 r4 g g fis g e | d2 r4 e e dis e c | b2 r2 r r4 a' |
  a gis a f e2 r | r4 e' e d e2 c | b1 r2 a | a gis a f | e1 r4 c' b a |
  g2 a4 f2( e4 d2) | c1 r | r4 c' b a2 g4 a f2( e4 d2) c1 |
  R\breve | r2 c' b a | g a f( e | d1) c2 r4 c' | d2 c b c | d( c1 b2) | c\breve
}

\putMusic choir.cantus.lyrics \lyricmode {
  Ein fe -- ste Burg ist un -- ser Gott, ist un -- ser Gott,
  ein fe -- ste Burg ist un -- ser Gott,
  ein fe -- ste Burg ist un -- ser Gott, ein gu -- te Wehr und Waf -- _ _ fen. Er hilft uns
  frei, er hilft uns frei, er hilft uns frei, die uns jetzt
  hat, die uns jetzt hat be -- trof -- fen, die
  uns jetzt hat be -- trof -- fen. mit Ernst er's jetzt
  meint, mit Ernst er's jetzt meint, der alt, __ der alt, __
  der alt __ bö -- se Feind mit Ernst er's jetzt meint, groß Macht und viel List, groß Macht und
  viel List, groß Macht und viel List, und viel List, groß Macht und viel List sein grau -- sam
  Rü -- stung ist, sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist,
  sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist, sein
  grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist; auf Erdn ist
  nicht seins glei -- chen, auf Erdn ist nicht seins glei -- chen,
  auf Erdn ist nicht seins glei -- chen, auf Erdn ist nicht seins glei -- chen.
}

\putMusic choir.tenor.music \relative c' {
  R\breve | r1 r4 c c c | g2 a4 c2( b4 a) a | g\breve |
  r1 r4 d f2.( e4 d) d c2 r4 c' | b2 a g a4 f2( e4 d2) c r |
  R\breve r1 r2 r4 g'2 f e4 f8.[( g16 a8. b16] c4) a | b4( c2 b4) c2 r4 c |
  c c g2 r4 g g g | d2 r4 d' d d a2 | r4 c c c g2 r4 a | c2.( b4 a) a g2 |
  r4 c b a g2 a4 f2( e4 d2) c r | r r4 c' b a g a | fis( g2 fis4) g1 |
  r2 g1 f1 e4( d) c2. c'4 | b2( c1 b2) | c1 r | r r2 r4 c, |
  g' a8 b c2 r1 | r r2 r4 c, | g' a8 b c2 r r4 g | b4.( c8 d2) r4 a c4.( d8 |
  e2) r r c1( bes4 c d2.) c4 | b!2 r4 a b1 | d2 d c r | r4 b c b a g r b |
  c b a g r e f e | d c8 c' b2 c r4 d | c2 g fis2( g1 fis2) g1 | r4 a a g a f e2 |
  r4 a a g a f e2 | r r4 d' d cis d b | a2 r r4 g g fis |
  g e d2 r r4 c' | c b c a g r r2 | r r4 e' e dis e c |
  b2 r4 a a gis a f | e2 r8 c' b a gis2 a | gis1 r2 c | c b cis d | cis1 r |
  R\breve | r4 c b a g2 a4 f2( e4 d2) c2. c'4 | b( c2 b4) c1 |
  r4 b g a fis g g( fis) | g2 r4 e' d2 c | b c d( c1 b2) c r4 c | b2 a g a | f( e d1) | c\breve
}

\putMusic choir.tenor.lyrics \lyricmode {
  Ein fe -- ste Burg ist un -- ser Gott,
  ist un -- ser Gott, ein gu -- te Wehr und Waf -- fen,
  ein gu -- te Wehr und Waf -- fen. Er
  hilft uns frei, er hilft uns frei, er hilft uns frei, er hilft uns frei, aus al -- ler Not,
  die uns jetzt hat be -- trof -- fen, die uns jetzt hat be -- trof -- fen,
  die uns jetzt hat be -- trof -- fen. mit
  Ernst er's jetzt meint, mit Ernst er's jetzt meint, der alt, __ der alt __
  bö -- se Feind mit Ernst er's jetzt meint, groß Macht und viel List, groß
  Macht und viel List, groß Macht und viel List, und viel List groß Macht und viel __ List sein grau -- sam Rü -- stung ist,
  sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist, sein grau -- sam
  Rü -- stung ist, sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung
  ist, sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist;
  auf Erdn ist nicht seins glei -- chen, seins glei -- chen,
  auf Erdn ist nicht seins glei -- chen, auf Erdn ist nicht seins glei -- chen, auf Erdn ist nicht seins glei -- chen.
}

\putMusic choir.bass.music \relative c {
  R\breve | R | R | r1 r4 c c c |
  g2 a4 c2( b4 a) a | e1 r | r r2 r4 a | c2.( b4 a) a g2 |
  r4 g'2 f4 e2 d | e4 c2( b4 a2) g | r c f,1( | g) c |
  r4 c c c g2 r4 g' | g g d2 r r4 f | f f c2 r4 e f2.( e4 d) d c1 |
  R\breve | R | r4 g' f e d2 e4 c2( b4 a2) g r4 c |
  d2( c4 b c2) d | g, c a1( | g\breve) | c2 r4 c e4.( f8 g4) a2( g) fis4 g1 ~ |
  g r4 g, a4.( b8 | c4) d2( c) b4 c1. r4 c e4.( f8 | g2) r4 d f4.( g8 a2) |
  r2 a( e fis | g2. f!8[ e] d4 cis d) d, | g2 a g1 | f2 g c r | r r4 b c b a g |
  r e' f e d c r c | g1 c2 g | c c d\breve g,1 | R\breve |
  r1 r4 d' d cis | d bes a2 r4 a' a gis | a f e2 r1 |
  r4 c c b c a g2 | r1 r2 r4 e' | e dis e c b2 r |
  r1 r2 r4 a' | a gis a f e\breve r2 a, | e' e a, a | a1 r2 r4 c' |
  b2 a g1 | a2 f( e d) | c r r1 | r r4 g' f e |
  d2 e4 c2( b4 a2) | g c d d | g,1. g2 | g1 c2 r4 a | d2 d g,1. g2 g1 | c\breve
}

\putMusic choir.bass.lyrics \lyricmode {
  Ein fe -- ste
  Burg ist un -- ser Gott, ist un -- ser Gott,
  ein gu -- te Wehr und Waf -- fen, und Waf -- fen.
  Er hilft uns frei, er hilft uns frei, er hilft uns frei aus al -- ler Not,
  die uns jetzt hat be -- trof -- fen, die
  uns __ jetzt hat be -- trof -- fen. Der alt __ bö -- se Feind, __
  der alt __ bö -- se Feind, der alt, __ der alt __
  bö -- se Feind mit Ernst er's jetzt meint, groß Macht und viel List,
  groß Macht und viel List, und viel List, groß Macht und viel List
  sein garu -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist,
  sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist,
  sein grau -- sam Rü -- stung ist, sein grau -- sam Rü -- stung ist; auf
  Erdn ist nicht seins glei -- chen, auf Erdn ist
  nicht seins glei -- chen, auf Erdn ist nicht seins glei -- chen, auf Erdn ist nicht seins glei -- chen.
}

% continuo music
\putMusic continuo \relative c {
  c1 c2 f4 e | d1 g,4 c c2 | b a d1 | g,4 c2 b4 c1 |
  g2 a4 c2 b4 a a | g1 c2 c' | b a g a4 a, | c2. b4 a2 g |
  g'2. f4 e2 d | e4 c2 b4 a2 g | c1 f, | g c |
  c g | g2 d' d1 | f2 c e f ~ | f4 e d2 c1 |
  c4 c' b a g2 a4 f ~ | f e d2 c g | c2. c4 g2 c4 a | d,1 g2 c |
  d b c d | g, c a1 | g\breve | c1 c | d e2 c |
  c2. e4 g g, a2 | c4 d2 c b4 c2 | c1 c2 c | g' d f a |
  a1 e2 fis | g2. e4 d1 | g,2 a g1 | f2 g c g | a4 g a b c b a g |
  a e' f e d c f c | g1 c2 g | c1 d1 ~ | d g, | f2 c' f, c' |
  f, c' f4 d d cis | d bes a2 a d4 e | cis d a2 d1 |
  g4 c, c b c a g2 | g c2. b4 e2 | e4 dis e c b2 e |
  e cis4 d e2 a,4 d | e2 a4 f e1 | e a, | e'2 e a, a | a1 c2. c'4 |
  b2 a g1 | a2 f e d | c g4 f2 e4 f2 | g1 c |
  g2 c4 a d,1 | g2 c d1 | g,\breve | g1 c2 a | d d g,1 | g2 g g1 | c\breve
}

% engrave headers (title) in a bookpart
\lalilyTest
