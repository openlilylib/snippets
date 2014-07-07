\version "2.18.0"
\include "templates/lalily/definitions.ily"

% init options
\optionsInit opts % ... we don't need them right now
% set current music folder and assign a template with options to it
\setDefaultTemplate music.choral.altatrinita lalily.vocal.group #opts
% set title for current (formerly set) music folder
\setTitle "Alta Trinita Beata"
% TODO set composer for current music folder with #'(name . life)
%\setComposer #'("Italy" . "15th Cent.")

% "meta-track" for this piece of music added to each staff!
% stored in #'(music choral altatrinita meta)
\putMusic meta {
  \key f \major \time 2/2
  \repeat volta 2 { s1*16 }
  \repeat volta 2 { s1*8 }
}

% soprano lyrics
% stored in music.choral.altatrinita.sop.lyrics
\putMusic sop.lyrics \lyricmode {
  Al -- ta Tri -- ni -- ta be -- a -- ta,
  da noi sem -- pre ad -- o -- ra -- ta,
  Tri -- ni -- ta glo -- ri -- o -- sa
  u -- ni -- ta ma -- ra vi -- glio -- sa,
  Tu -- sei man -- na sa -- po -- ro -- sa
  e tut -- ta de -- si -- de -- ro -- sa.
}
% soprano music
% stored in music.choral.altatrinita.sop.music
\putMusic sop.music \relative c' {
  f2 f4( g) | a2 g4( f) | bes2 a4( g) | a2 a |
  g2 a4( bes) | a2 g4( f) | g( bes) a( g) | f2 f \breathe |
  c'2 c4( d) | bes1 | c4( bes) a( g) | a2 a |
  g4( f) bes( g) | f2 bes4( a) | g( f) bes( g) | f2 f |
  c'2 c4( d) | bes2 c4( d) | ees( d) c( a) | bes2 g |
  f2 g4( a) | bes2 a | g4( f) bes( g) | f2 f |
}
% alto lyrics - reusing soprano lyrics
\putMusic alt.lyrics \getMusic sop.lyrics
% alto music
\putMusic alt.music \relative c' {
  c2 c4( e) | f2 e4( f) | f2 f4( e) | f2 f |
  e2 f | f e4( f) | e( f) f( e) | c2 c \breathe |
  a'2 a | g1 | g2 f4( e) | f2 f |
  e4( d) d( e) | f2 f | e4( d) d( e) | c2 c |
  a'2 a4( f) | g2 g4( bes) | c( bes) g( f) | f2 e |
  c2 e4( f) | f2 f | e4( d) d( e) | c2 c |
}
% tenoro lyrics - reusing soprano lyrics
\putMusic ten.lyrics \getMusic sop.lyrics
% tenor music
\putMusic ten.music \relative c' {
  a2 a4( c) | c2 c | d c | c c |
  c2 c4( d) | c2 c4( a) | c( d) c2 | a a \breathe |
  f'2 f | d1 | e2 c | c c |
  c4( a) bes( c) | c2 d4( c) | c( a) bes( c) | a2 a |
  f'2 f | d ees4( f) | g( f) ees( c) | d2 c |
  a2 c | d c | c4( a) bes( c) | a2 a |
}
% basso lyrics - reusing soprano lyrics
\putMusic bas.lyrics \getMusic sop.lyrics
% basso music
\putMusic bas.music \relative c {
  f2 f4( c) | f2 c4( f) | bes,2 f'4( c) | f2 f |
  c2 f4( bes,) | f'2 c4( d) | c( bes) f'( c) | f,2 f \breathe |
  f'2 f4( d) | g1 | c,2 f4( c) | f2 f |
  c4( d) g,( c) | f2 bes,4( f') | c( d) g,( c) | f2 f |
  f2 f4( d) | g2 ees4( d) | c( d) ees( f) | bes,2 c |
  f2 c4( f) | bes,2 f' | c4( d) g,( c) | f,2 f |
}

\addEdition sheet
\editionMod sheet 17 0/1 StaffGroup.A \once \override BarLine.allow-span-bar = ##t
\editionMod sheet 25 0/1 StaffGroup.A \once \override BarLine.allow-span-bar = ##t

% instantiate this piece of music ... creates PDF, MIDI and a log file
% this only happens, if this file is compiled directly and not included!
% To check, if this is the case, the current outputname is compared to the current file name.
% If the outputname is changed, this may not work!
\lalilyTest
