% Content for the Beethoven font example

% First define a few tweaks as commands

doubleSlursOnce = \once \set doubleSlurs = ##t

whiteFFMarkup = \markup { \whiteout \pad-markup #0.5 \dynamic ff}
whiteFF = #(make-dynamic-script whiteFFMarkup)

clefFix = {
  \once \override Staff.Clef.extra-offset = #'(0.5 . 0)
  \once \override Staff.Clef.padding = #2
}



OpXNoIII_piano_global = {
  \key d \major
  \time 2/2
  \tempo \markup "Presto"
  \partial 4 s4
  s1*9
  \newSpacingSection
  s1*6
  \newSpacingSection
  s1*7
  \newSpacingSection
  s1*10
}

OpXNoIII_piano_notes_upper = \relative d' {
    d4(   | % 0
  cs4-.) b-. a-. cs-.   | % 1
  d4-. fs-. <a, a'>-. <cs cs'>-.   | % 2
  <d d'>4-. <e e'>-. <fs fs'>-. <g g'>-.   | % 3
  <a a'>2.\fermata \voiceTwo d4\(   | % 4
  cs4 b a << b \new Voice { \voiceOne a'4 } >>   | % 5
  << { a,4 g fs g } \new Voice { \voiceOne a'2. } >>   | % 6
  fs,4 e d << e \new Voice { \voiceOne d'4 } >>   | % 7
  \oneVoice <d, d'>4^( <cs cs'> <d d'>)\) \doubleSlursOnce <ds ds'>( ~   | % 8
  <ds ds'>4 <e e'>) << { \voiceOne cs'4-. a-. } \new Voice { \voiceTwo g4 } >>   | % 9
  << d'!4 \new Voice { \voiceTwo fs, } >> \oneVoice r r fs'8 d'   | % 10
  e,8 cs' d, b' cs, a' d, b'   | % 11
  cs,8 a' b, g' a, fs' b, g'   | % 12
  a,8 fs' g, e' fs, d' g, e'   | % 13
  fs,8 d' fs, ds' g, e' a, fs'   | % 14
  b,8 g' fs a g b <e,, g cs>4-.   | % 15
  <d fs d'>4 r r r8 d   | % 16
  r8 cs r b r a r cs   | % 17
  r8 d r fs r a r <cs, cs'>   | % 18
  r8 <d d'> r <e e'> r <fs fs'> r <g g'>   | % 19
  r8 <a a'> r <as as'> r <b b'> r <cs cs'>   | % 20
  <d d'>4-. r <e e'>-. r   | % 21
  <fs fs'>2.\fermata fs,4   | % 22
  d'2( fs)   | % 23
  d2( e8 d cs b)   | % 24
  as4-. as-. cs8( b as b)   | % 25
  cs2( fs,4-.) fs-.   | % 26
  d'2( fs)   | % 27
  d2( e8 d cs b)   | % 28
  a4-. a-. a8( gs fs gs)   | % 29
  fs4 r r cs   | % 30
  gs'8 cs, es gs a cs, fs a   | % 31
  b8 cs, es gs cs b a gs   | % 32
}

OpXNoIII_piano_dynamics = {
    s4-\tweak #'X-offset #-1 \p   | % 0
  s1*3   | % 1-3
  s2.-\tweak #'Y-offset #0.5 \sf s4\p   | % 4
  s1*5   | % 5-9
  s2. s4\f   | % 10
  s1*5   | % 11-15
  s2. s4\p   | % 16
  s1   | % 17
  s4 s2.-\tweak #'Y-offset #-0.7 \cresc   | % 18
  s1*2   | % 19-20
  s2\ff s\ff   | % 21
  s2.-\tweak #'vertical-skylines #f -\tweak #'horizontal-skylines #f
      -\tweak #'X-offset #-3
      -\tweak #'extra-spacing-width #empty-interval
      -\tweak #'extra-spacing-height #empty-interval \whiteFF
    s4\p   | % 22
}

OpXNoIII_piano_notes_lower = \relative d, {
  \clef bass
  <<
    { \voiceTwo d4( }
    \new Voice { \voiceOne \crossStaff { d'4 } }
  >> \oneVoice   | % 0
  cs,4-.) b-. a-. cs-.   | % 1
  d4-. fs-. <a, a'>-. <cs cs'>-.   | % 2
  <d d'>4-. <e e'>-. <fs fs'>-. <g g'>-.   | % 3
  <a a'>2.\fermata \clef treble <fs'' a>4_(   | % 4
  <e g>4 <d fs> <cs e> <d fs>   | % 5
  \clefFix \clef bass <cs e>4 <b d> <a cs> <b d>   | % 6
  <a cs>4 <g b> <fs a> <g b>   | % 7
  <fs a>4 <e g> <d fs>) r4   | % 8
  <g, g'>4 r <a a'> r   | % 9
  <d, d'>4 r r2   | % 10
  r2 r4 <a a'>-.^\sf   | % 11
  <a a'>1 ~   | % 12
  <a a'>2. <as as'>4-.   | % 13
  <b b'>4-. <a! a'!>-. <g g'>-. <fs fs'>-.   | % 14
  <e e'>4-. r <a a'>-. r   | % 15
  <d d'>4 r r <<
   { \voiceOne s8 \crossStaff { d' | s8 cs s \parenthesize b s \parenthesize a s cs | s8 d s fs s a } }
   { \voiceTwo \stemUp d,,4-. | cs4-. b-. a-. cs-. | d4-. fs-. <a, a'>-. }
  >> \oneVoice <cs cs'>-.   | % 16-18
  <d d'>4-. <e e'>-. <fs fs'>-. <g g'>-.   | % 19
  <a a'>4-. <as as'>-. <b b'>-. <cs cs'>-.   | % 20
  <d d'>4-. r <e e'>-. r   | % 21
  <fs fs'>2.-\tweak #'layer #2 \fermata r4   | % 22
  b,8 d fs b as, e' fs cs'   | % 23
  b,8 d fs b b, d fs b   | % 24
  cs,8 e fs as b, ds fs b   | % 25
  fs,8 cs' fs as fs, cs' fs as   | % 26
  b,8 d fs b as, e' fs cs'   | % 27
  b,8 d fs b d, fs b fs   | % 28
  cs8 fs a fs cs es gs es   | % 29
  fs,8 a cs fs fs,4 r   | % 30
  <<
    { \voiceTwo cs'1 ~ cs4 }
    \new Voice { \voiceOne a'4\rest <gs b>2 ( <fs a>4 <es gs>4) }
  >> \oneVoice r4 r2   | % 31-32
}
