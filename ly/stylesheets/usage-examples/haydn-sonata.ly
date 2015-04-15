\version "2.18.2"
\language "english"
#(set-global-staff-size 18)
\include "openlilylib"
\useLibrary stylesheets
\useNotationFont Haydn

\paper {
  paper-height = 8\in
  top-markup-spacing.basic-distance = 5
  markup-system-spacing.basic-distance = 15
  systems-per-page = 4
}

\header {
  title = \markup { \huge \larger \larger \larger \larger "S O N A T E" }
  composer = "Joseph Haydn"
  subtitle = "No. 37"
}

global = {
  \set Staff.connectArpeggios = ##t
  \key f \major
  \time 3/4
  \tempo "Largo e sostenuto."
  s2. \noBreak s \noBreak s \break
  s2. \noBreak s \noBreak s \noBreak s \noBreak s \noBreak s \bar ":|." \break
  s2. \noBreak s \noBreak s \noBreak s \break
  s2. \noBreak s \noBreak s \noBreak s \noBreak s \noBreak s \bar "|."
}

fourToFiveUp = \finger \markup { 
  \override #'(baseline-skip . 1.2)
  \center-column { 
    \scale #'(1 . -1) \musicglyph #"ties.lyric.default" 
    "4 5"
  } 
} 
twoToOneUp = \finger \markup { 
  \override #'(baseline-skip . 1.2)
  \center-column { 
    \scale #'(1 . -1) \musicglyph #"ties.lyric.default" 
    "2 1"
  } 
} 
threeToFiveDown = \finger \markup { 
  \override #'(baseline-skip . 0.1)
  \center-column { 
    "3 5"
    \musicglyph #"ties.lyric.default" 
  } 
} 
pad = \once \override Fingering.staff-padding = #'()
fingerLeft = \set fingeringOrientations = #'(left)
fingerUp = { \pad \set fingeringOrientations = #'(up) }
fingerRight = \set fingeringOrientations = #'(right)
fingerDown = { \pad \set fingeringOrientations = #'(down) }

ten = \markup \italic \center-align "ten."
moveup = \change Staff = up
movedn = \change Staff = dn

rhVI = \relative c' {
  \clef treble
  \voiceOne
  \tupletDown
  
  \once \override DynamicText.X-offset = #-2
  \once \override DynamicText.Y-offset = #-5
  d4\f f4.-5 e8-4 |
  \once \override Tie #'staff-position = #-3
  d4-3 ~ d16 \tuplet 3/2 { 
    \once \override Slur.positions = #'(-4.5 . -3.5)
    d32-1_( e fs ) } g16. [ a32-5 ] bf4\fourToFiveUp
  bf4 a8.-5 \tuplet 3/2 { g32_( f e ) } f4-4 |
  
  \appoggiatura { \once \override Fingering.font-size = #-5 e32-3 f g } 
    f4-4 e2-\ten |
  f4-1-4 g4.-3-5 g8 |
  g8. \tuplet 3/2 { c,32_( d e ) } f16. e32-2 f16. g32 a4 |
  a8-1-3 ( g ) <g bf d>4\arpeggio <bf d>8 ( <a c> ) |
  <a c>8 ( <g bf> ) q ( <f a> ) q ( <e g> ) |
  \shape #'((0 . 0) (0 . -0.5) (0 . -0.5) (0 . 0)) Slur
  <e g>4 ( <f a>2-\ten ) |
  
  <fs a>4-4 <g bf>8.-5 a32 g fs8 a |
  a8.-5 \tuplet 3/2 { 
    \once \override Slur.positions = #'(-4.5 . -3.5)
    d,32_( e fs ) } g16. fs32-2 g16.-3 a32 bf4 |
  bf8-4 a a4 g-3 |
  a2 d8\rest bf8-2-4 |
  
  <g bf>8. <f a>16 q4 bf8\rest <g bf>\pp |
  <g bf>8. <f a>16 q4-2-4\< <d fs a>-3-5\! |
  <bf_~ ef_~ g^~ bf^~>4\ff q8. \tuplet 3/2 { a'32-5\>_( g f ) } ef!8 d |
  \once \override DynamicText.X-offset = #-1.5
  \once \override DynamicText.Y-offset = #-2
  \shape #'((0 . 0) (0 . -0.35) (0 . -0.35) (0 . 0.1)) Slur
  \fingerUp <d-5>4\p ( \fingerUp <cs-4>8 ) \movedn <f, a>-3-5 <e g a> <d f a> |
  <cs e a>4 q q |
  <cs e a>2.\fermata 
}

rhVII = \relative c' {
  \clef treble
  \voiceTwo
  \tupletUp
  
  s4 \fingerLeft <a-2>8. \fingerUp <b-1>16 cs4 |
  \once \override Tie.details.height-limit = #4 
  d2 ~ d16 \fingerUp <e-2>32 f e16 [ d ] |
  cs4-2 \fingerLeft <d-1>8 e \fingerDown <e-1> d |
  
  d4-1 cs2 |
  d4 e8. \tuplet 3/2 { 
    \once \override Slur.positions = #'(1.2 . 0.3)
    f32^( e d ) } c4 |
  c2 f4 |
  f4 <e>\arpeggio f4 |
  d4 c c |
  c2. |
  
  d4 d d |
  d2 g4 |
  g4 g8 f f e |
  e8. f16 f4 r8 g |
  
  s2.*6 |
  
}

lhVI = \relative c {
  \clef bass
  \voiceOne
  \tupletDown
  \crossStaff { <f a>4 } f8.-2 g16-1 a4-1 |
  a8. \tuplet 3/2 { 
    \once \override Slur.positions = #'(-2.1 . -1.1)
    d,32-4_( e fs ) } g2 ~ |
  g8. \tuplet 3/2 { e32-3_( f g ) } f8 cs-4 d4-1 |
  
  a4\twoToOneUp a,2 |
  a''4-1 bf2-1 |
  bf4-2 \fingerDown <a-1>16. \fingerDown <g-3>32 a16. bf32 a16.-1 g32 f16. e32 |
  d4 s2 |
  s2. |
  bf'4 ( a2 ) |
  
  <fs a>4 <bf, d g> c'8. \tuplet 3/2 { 
    \once \override Slur.positions = #'(-0.3 . 0.5) 
    a32-3_( bf c ) } |
  c4 bf16. a32 bf16. c32 bf16. a32-1 g16. f32-1 |
  e8 a a-2 b b-2 cs |
  cs8.-2 d16 d4 r8 cs |
  
  cs8. d16 d4 r8 cs |
  cs8. d16 d4 c4 |
  \oneVoice <g, bf ef g>2 ~ q8 <gs f'> |
  \voiceOne \crossStaff { f'4 ( e8 ) } s4. |
  s2.*2 |
}

lhVII = \relative c, {
  \clef bass
  \voiceTwo
  <d f a d>4 d'-4 a |
  bf2-5 g4-5 |
  a2-5 ~ a8 \fingerUp <bf-2>16. \fingerUp <gs-3>32 |
  
  s2. |
  d'4-4 c!-5 e\threeToFiveDown |
  f2-4 f16.-3 e32 d16.-3 c32 |
  bf4 <bf c e g>\arpeggio <a c f>\arpeggio |
  <bf d g>4\arpeggio <c f a>\arpeggio <c g' bf>\arpeggio |
  f2. |
  
  d4-4 g, d'-5 |
  g2-4 g16. f!32-3 e16. d32-3 |
  cs4 d-5 e-5 |
  f2-5 r8 e |
  
  f2 r8 e |
  f2 fs4 |
  s2. |
  <a,, a'>2. |
  <a a'>4 q q |
  <a a'>2.\fermata |
}

\score {
  \new PianoStaff <<
    \new Staff = up <<
      \global
      \new Voice \rhVI
      \new Voice \rhVII
    >>
    \new Staff = dn <<
      \global
      \new Voice \lhVI
      \new Voice \lhVII
    >>
  >>
  \layout {
    \context {
      \Staff
      \consists "Span_arpeggio_engraver"
      \override TupletBracket.staff-padding = #'()
      \omit TupletBracket
      \override TupletNumber.font-name = #"Century Schoolbook L Bold Italic"
      beamExceptions = #'()
      beatStructure = #'(1 1 1)
      \override BarLine.space-alist.next-note = #'(fixed-space . 2.0)
    }
    \context {
      \PianoStaff
      \consists #Span_stem_engraver
      \override VerticalAxisGroup.staff-staff-spacing = #'((basic-distance . 0)
                                                           (minimum-distance . 0)
                                                           (padding . 0.25))
    }
    \context {
      \Score
      \remove "Bar_number_engraver"
      \override MetronomeMark.font-size = #1.1
      \override MetronomeMark.stencil = 
        #(lambda (grob)
           (ly:stencil-scale (ly:text-interface::print grob) 1.25 1))
      \override Arpeggio.before-line-breaking =
        #(lambda (grob)
           (let* ((pos (ly:grob-property grob 'positions))
                  (p1 (car pos))
                  (p2 (cdr pos))
                  (p1a (- p1 0.75))
                  (p2a (+ p2 0.25))
                  )
             (ly:grob-set-property! grob 'positions (cons p1a p2a))
           ))
    }
  }
  \midi { \tempo 4 = 40 }
}