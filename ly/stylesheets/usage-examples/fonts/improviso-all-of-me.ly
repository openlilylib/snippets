\version "2.18.2"

#(set-global-staff-size 18)

\include "openlilylib"
\useLibrary Stylesheets
\useNotationFont \with {
  extensions = ##t
}
LilyJAZZ

%\include "LilyJAZZ.ily"
%\include "jazzchords.ily"

\paper {
%  #(set-paper-size "letter")
%  paper-height = 11\in
%  paper-width = 8.5\in
  indent = 0\mm
  between-system-space = 2.5\cm
  between-system-padding = #0
  %%set to ##t if your score is less than one page:
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  markup-system-spacing = #'((basic-distance . 23)
                             (minimum-distance . 8)
                             (padding . 1))
}

title = "All Of Me"
composer = "-Simons & Marks"
meter = " (Med. Swing)"

realBookTitle = \markup {
  \score {
    {
      \override TextScript.extra-offset = #'(0 . -4.5)
      s4
      s^\markup {
        \fill-line {
          \fontsize #1 \lower #1 \rotate #7 \concat { " " #meter }
          \fontsize #8
            \override #'(offset . 7)
            \override #'(thickness . 6)
            \underline \sans #title
          \fontsize #1 \lower #1 \concat { #composer " " }
        }
      }
      s
    }
    \layout {
      \once \override Staff.Clef.stencil = ##f
      \once \override Staff.TimeSignature.stencil = ##f
      \once \override Staff.KeySignature.stencil = ##f
      ragged-right = ##f
    }
  }
}

\header {
  title = \realBookTitle
  tagline = ##f
}

theNotes = \relative c' {
  \set Staff.midiInstrument = "flute"
  \key c \major
  \showStartRepeatBar \bar "[|:"
  \repeat "volta" 2 {
    c'4 g8 e ~ e2 ~
    e2 \times 2/3 { c'4 d c }
    b4 gis8  e ~ e2 ~
    e1			\break
    a4. g8 e2 ~
    e4 dis e8 bes' a4
    g2 f2 ~
    f1			\break
    e4. ees8 d2 ~
    d2  e8 gis c4
    d2 c2 ~
    c1			\break
    b4. bes8 a2 ~
    a2 a8 d b4
    a1
    b1	\bar "||"	\break

    c4 g8 e ~ e2 ~
    e2 \times 2/3 { c'4 d c }
    b4 gis8  e ~e2 ~
    e1			\break
    a4. g8 e2 ~
    e4 dis e8 bes' a4
    g2 f2 ~
    f1			\break
  }
  \alternative {
    {
      d'2 c4 b
      d2. c4
      b2 e,4  g4
      b2. a4		\break
      c2 a4 c
      e2 e2
      c1 ~
      c1 \bar ":|][|:"	\break
    }
    {
      d2 c4 b
      d2. c4
      b2 e,4 g4
      b2. a4		\break
      c2 a4 c
      e2 e2
      c1
      ~c1
      \bar ":|]"
    }
  }
}

theChords = \chordmode {
  \repeat "volta" 2 {
    c1:maj	c1:maj		e:7		e:7		|
    a:7		a:7		d:m7		d:m7 		|
    e:7		e:7		a:m7		a:m7		|
    d:7		d:7		d:m7		g:7		|
    c1:maj	c1:maj		e:7		e:7		|
    a:7		a:7		d:m7		d:m7	 	|
  }
  \alternative {
    {
      f1	f:m	c2:maj	e:m7	a1:7		|
      d:m7	g:7	c2:6	ees:dim	d2:m7	g:7 	|
    }
    {
      f1	f:m	c2:maj	e:m7	a1:7		|
      d:m7	g:7	c1:6 		c1:6		|
    }
  }
}

theWords = \lyricmode {
  \repeat "volta" 2 {
    All of me
    Why not take all of me
    Can't you see
    I'm no good with -- out you

    Take my lips
    I want to lose them
    Take my arms
    I ne -- ver use them

    Your Good -- bye
    Left me with eyes that cry
    How can I go on dear with -- out you
  }
  \alternative {
    {
      You took the part
      That once was my heart
      So why not take all of me
    }
    {
      You took the best
      So why not take the rest
      Ba -- by take all of me.
    }
  }
}

\score {
  <<
    \new ChordNames \theChords
    \new Voice = chant \theNotes
    \new Lyrics \lyricsto chant \theWords
  >>
  \layout {
    \override Score.Clef #'break-visibility = #'#(#f #f #f)  % make only the first clef visible
    \override Score.KeySignature #'break-visibility = #'#(#f #f #f)  % make only the first time signature visible
    \override Score.SystemStartBar #'collapse-height = #1  % allow single-staff system bars
  }
  \midi {
    \tempo 4 = 88
  }
}
