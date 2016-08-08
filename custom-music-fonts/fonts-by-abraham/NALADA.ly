\version "2.18.2"
\include "english.ly"

% NOTICE THE CHANGE IN STAFF SIZE PRIOR TO CHANGING THE MUSIC FONT
#(set-global-staff-size 17)

% UNCOMMENT THIS TO USE THE DEFAULT STYLESHEET FOR THE MUSIC FONT
% (REMEMBER TO ADD THE DIRECTORY OF THIS STYLESHEET TO LILYPOND'S PATH)
%\include "profondo.ily"

% UNCOMMENT THIS PAPER BLOCK TO ONLY CHANGE THE FONTS 
% (NO OTHER STYLES ARE CHANGED, AND NO SPECIAL INCLUDES ARE REQUIRED)
\paper {
  #(define fonts
     (make-pango-font-tree
      "profondo"
      "emmentaler"
      "Century Schoolbook L"
      "sans"
      "monospace"
      (/ staff-height pt 20)))
}

% NOTE: IF BOTH THE ABOVE SECTIONS ARE UNCOMMENTED, THEN THE 
% PAPER BLOCK WILL CHANGE THE FONT AND THE INCLUDE BLOCK WILL 
% ONLY APPLY STYLE/LAYOUT OVERRIDES (i.e., THE ORDER MATTERS)

\paper {
  #(set-paper-size "letter")
  top-margin = 0.3\in
  bottom-margin = 0.35\in
  left-margin = 0.85\in
  right-margin = 0.8\in
  indent=0.25\in  % make this 0 to take out the first indent
  ragged-last-bottom = ##f
  page-count = #1
}

\header {
  title = \markup{ \normal-text \larger "NÁLADA" }
  composer = "FIBICH, Op.41 No. 139"
  tagline = ##f
}

global = {
  \tempo "Lento" 4. = 40
  \time 12/8
  \key df \major
}

% Change staff
cu = { \once \change Staff = "upper" }
cl = { \once \change Staff = "lower" }

ppEspress = \markup { \hspace #10 \dynamic pp \normal-text \italic espress. }
ppEspress = #(make-dynamic-script ppEspress)

myBreak = { \break }

pianoRH = <<
  \clef treble
  \relative c' {
    \partial 2. e4.--^\markup{ \italic "molto cantabile" }( f-- |
    bf2.->) ef,!8[(^\< f8 af8]\! gf8[^\> f8 e8]\! |
    gf8-.) f4->~ f4. g4.->( af4.-> |
    <a df>4.~^\sfz <a df>4) <a c>8 <df, f a>8_-[ <df f bf>8_- <f df' f>8_-] \once \stemDown <ef c' ef>4(-> <df bf' df>8) |
    <df df'>8( <c_~ c'^~>4 <c c'>4.) s4. s4. |
    s1. |
    s1. |
    df''8\arpeggio[( c8 f,8] af4.) af8\arpeggio[( ef8 f16 af16] gf8[ f8 e8] |
    gf8) f4~ f4. s4. s4. |
    df8\arpeggio[( c8 f,8] af4.) af8\arpeggio[( ef8 f16 af] gf8[ f8 e8] |
    gf8->) f!4~ f4. \bar "|."
  }
  \\
  \relative c' {
    \partial 2. r8 <af df>8[ <af df>8] r8 <af df>8[ <af df>8] |
    r8 <bf df f>8[ <bf df f>8] <bf df f>8[ <bf df f>8 <bf df f>8] <gf bf>8[  <gf bf>8 <gf bf>8] <gf c>8[ <gf c>8 <gf c>8] |
    <af df>8[ <af df>8 <af df>8] <af df>8[ <af df>8 <af df>8] r8 <g df' ef!>8[ <g df' ef>8] r8 <af df f>8[ <af df f>8] |
    r8 <ef' f>8[ <ef f>8] <ef f>8[ <ef f>8 f8] s4. s4. |
    <gf bf>4.~( <gf bf>8[ <f af!>8 <ef gf>8]) \oneVoice \acciaccatura f8 <df' f>4. \oneVoice \acciaccatura f,8 <df' f>4. |
    \oneVoice \acciaccatura f,8 <df' f>8[( <c ef>8 <a c>8] \oneVoice \acciaccatura f8 <bf df>8[ <c ef>8 <df f>8]) <bf gf' bf>4. <bf gf' bf>4. |
    \voiceTwo <bf gf' bf>8^>[ <af! f' af!>8^> <gf' bf gf'>8^>] <gf bf gf'>8^>[ <f af f'>8^> <ef gf ef'>8^>] <df f df'>4. <df ef bff' df>4. |
    <df f af>4.\arpeggio df8[( c8 f,8]) <gf c>2.\arpeggio |
    <af df>2. \oneVoice \grace { df,16[( f16 bf16] } df4.) \grace { df,16[^( ef!16 bff'16] } df4.) |
    \voiceTwo <df, f af>4.\arpeggio df8[ c8 \cl \once \stemUp f,8] \cu <gf c>2\arpeggio s4 <af df>2 s4 \bar "|."
  }
>>
  
pianoLH = \relative c, {
  \clef bass
  \partial 2. <df df'>4. <c c'>4. |
  <bf bf'>2. <ef ef'>4. <af, af'>4. |
  \myBreak
  <df df'>2. <bf bf'>4. <af af'>4. |
  <f f'>2. <bf bf'>4. << { r8 g''4^> } \\ ef,4. >> |
  \myBreak
  <<
    \relative c' {
      af8[ af8 af8] af8[ af8 af8] r8 <f af df>8[ <af df f>8] r8 <f bf df>8[ <bf df f>8] |
      s4. s4. r8 <gf bf ef>8[ <bf gf'>8] r8 <gf bf ef>8[ <bf gf>8] |
    }
    \\
    \relative c {
      af2. <df, df'>4~ <df df'>16[ <c c'>16] <bf bf'>4~ <bf bf'>16[ <af af'>16] |
      <gf gf'>8[ <gf'' bf df>8 <gf bf df f>8] <bf,, bf'>8[ <bf'' df f>8 <f bf df>8] <e, e'>4~ <e e'>16[ <df df'>16] <c c'>4~ <c c'>16[ <bf bf'>16] |
    }
  >>
  \myBreak
  <af, af'>8[ <gf'' af c ef>8 <af c ef gf>8] <af,, af'>8[ <af'' c af'>8 <a,, a'>8] <bf bf'>8[ <f'' bf df>8 <bf df f>8] \once \stemDown <gf,, gf'>8[ <gf'' bff df ef>8 <bff df ef gf>8] |
  <af,! f' af! df>2.\arpeggio <af ef' af ef'>2.\arpeggio |
  \myBreak
  <df, df'>8[ <af'' df f>8 <af df f>8] <af df f>8[ <gf af ef'> <f af df>] <bf,, bf'>8[ <f'' bf df>8 <bf df f>8] <gf,, gf'>8[ \once \stemDown <gf'' bff ef>8 <bff ef gf>8] |
  <af, f' af>2. <af, af'>2. |
  <df af'>2. \bar "|."
}

dynamics = {
  \partial 2. s4.\pp\< s4. |
  s8 s8\! s8 s4. s2. |
  s2. s4.\mf s4. |
  s2. s4.\< s4.\f |
  s2.\> s8\p s8 s16 s16\< s8 s4\! |
  s2.\< s4.\f s4.\< |
  s4 s8\ff s4.\sfz s2.\p |
  s2.\ppEspress s4\< s8\! s4\> s8\! |
  s4.\> s8\! s4 s2.\pp|
  s2. s4\< s16 s16\! s4\> s8\! |
  s8\> s4 s4.\!
}

pedals = {
  \partial 2. s4.\sustainOn s4.\sustainOff\sustainOn |
  s8\sustainOff\sustainOn s4 s4. s4.\sustainOff\sustainOn s4.\sustainOff\sustainOn |
  \override TextScript #'extra-offset = #'(1 . -1.6)
  s8\sustainOff\sustainOn^\markup{ \italic "Ped. simile" } s8 s8 s4. s2. |
  \revert TextScript #'extra-offset
  s1. |
  s1. |
  s1. |
  s1. |
  s2.\sustainOn s4\sustainOff\sustainOn s8 s4. |
  s4.\sustainOff\sustainOn s8 s8\sustainOff\sustainOn s8\sustainOff\sustainOn s4.\sustainOff\sustainOn s4.\sustainOff\sustainOn|
  s2.\sustainOff\sustainOn s4\sustainOff\sustainOn s2 |
  s8\sustainOff\sustainOn s8\sustainOff\sustainOn s2
  
}
\score {
  \new PianoStaff <<
    \new Staff = "upper" \with {
      \consists "Span_arpeggio_engraver"  % required for cross-voice arpeggios
    }  {
      \set Staff.connectArpeggios = ##t  % required for cross-voice arpeggios within a single staff
      \global
      \pianoRH
    }
    \new Dynamics {
      \dynamics
    }
    \new Staff = "lower" {
      \global
      \pianoLH
    }
    \new Dynamics {
      \set Dynamics.pedalSustainStyle = #'mixed
      \pedals
    }
  >>
  \layout {}
}
