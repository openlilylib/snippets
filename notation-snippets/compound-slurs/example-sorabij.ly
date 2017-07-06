\version "2.19.48"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.compound-slurs
%\include "compound-slurs.ily"

\loadPackage scholarly
\loadModule scholarly.annotate

#(set-default-paper-size "a3" 'landscape)

\paper {
  system-count = 2
  indent = 0
  system-system-spacing.minimum-distance = 40
  markup-system-spacing.minimum-distance = 24
}

\header {
  title = "from: “Opus Clavicembalisticum”"
  subtitle = "after: http://homes.soic.indiana.edu/donbyrd/InterestingMusicNotation.html"
  composer = "Kaikhosru Sorabij"
  opus = "(1930)"
}

exampleSlur = ^\compoundSlur \with {
  annotate = ##t
%  show-original-slur = ##t
  show-grid = ##t

  start-point = #'(0 . 3)
  start-angle = 55
  start-ratio = 0.5
  end-point = #'(2.5 . 4)
  end-angle = 75
  end-ratio = 0.5

  inflection =
  #'((X-ratio . .215)
     (Y-offset . 22)
     (angle . -35)
     (ratio-left . 0.7)
     (ratio-right . 0.22))

  inflection =
  #'((X-ratio . .25)
     (Y-offset . -9)
     (angle . 35)
     (ratio-left . .55)
     (ratio-right . .4))

  inflection =
  #'((X-ratio . .33)
     (Y-offset . -16)
     (angle . 20)
     (ratio-left . .45)
     (ratio-right . .4)
     (label . "A"))

  inflection =
  #'((X-ratio . .49)
     (Y-offset . -3)
     (angle . 25)
     (ratio-left . .45)
     (ratio-right . .15))

  inflection =
  #'((X-ratio . .65)
     (Y-offset . 26)
     (angle . -38)
     (ratio-left . .6)
     (ratio-right . .4)
     (label . "B"))

  inflection =
  #'((X-ratio . .72)
     (Y-offset . 21)
     (angle . -15)
     (ratio-left . .3)
     (ratio-right . .4))

  inflection =
  #'((X-ratio . .735)
     (Y-offset . -10)
     (angle . 30)
     (ratio-left . .35)
     (ratio-right . .2))

}

structure = {
  \omit Staff.TimeSignature
  \time 5/4
  s4*5
  \bar ""
  \time 1/8
  s8
  \bar "!"
  \time 3/4
  s2.

  \bar ""
  \break
  \time 5/4
  s4*5
  \bar "!"

  \time 2/4
  s2
}

voiceI = \relative b' {
  % TODO: Phrasing slur
  \musicalIssue \with {
    author = "Urs Liska"
    message = "First quaver is dotted in OE, so either the 10-tuplet must be 128th notes
or the dot removed. However, horizontal alignment suggest that the dot on the first note is wrong"
  footnote-offset = #'(-1 . 3)
  } NoteHead
  b8-- \mp [ \( \tuplet 10/8 {
    c64 _( d es d es d c b a b ]
  }
  a4. ) bes32 ( c a bes )
  gis8-- [ fis-- e-- d-- c-- ]

  |
  \tuplet 3/2 {
    bes64
    \change Staff = "three"
    as a
  }
  bes8..
  \change Staff = "two"
  d8 \tuplet 12/8 {
    e64 d e d  e f g f  g as g e!
  }
  f4 ~

  |
  f8 [ e d c ]
  \change Staff = "three"
  \once \override Slur.color = #red
  b4 ~ b64 c
  \change Staff = "two"
  d64 e b c es f
  fis128 [
  \musicalIssue \with {
    author = "Urs Liska"
    message = "Accidental is missing but seems obvious."
    footnote-offset = #'(1 . -11)

  }
  Accidental
  e fis16.. a8 ) cis ]

  |
  \tuplet 5/4 {
    e,64-> d e fis f
  }
  \musicalIssue \with {
    author = "Urs Liska"
    message = "OE prints double dotted eighth note. Either the additional dot is wrong 
or the preceding group should be 128th notes. However, horizontal alignment with lower voice
suggests our reading."
    footnote-offset = #'(0 . -1.2)
  } NoteHead
  e8.
  \musicalIssue \with {
    author = "Urs Liska"
    message = "OE prints dotted eighth note. Either the dot is wrong or the
following group should be 128th notes. However, horizontal alignment with lower voice
suggests our reading."
    footnote-offset = #'(0.5 . -1.2)
  } NoteHead
  d8 e64 d c d  es e d e \)

  |
}

voiceII = \relative b' {
  \voiceTwo
  \mergeDifferentlyDottedOn
  b4 a2
  s8
  \tuplet 9/8 {
    fis64 ( e fis  e d c  d e fis
  }
  e128 d e16.. )
  s8 \tuplet 5/4 {
    c32 b c d c
  }

  |
  bes4 s2
}

voiceIII = \relative fis,, {
  \tuplet 6/4 4 {
    \voiceTwo
    r16 fis \exampleSlur
    b fis' b fis'
    b \change Staff = "two" fis' b fis' \change Staff = "one" b fis'
    b, \change Staff = "two" fis b, fis \change Staff = "three" b, fis
    \oneVoice
    b, fis b, fis b fis'
    \voiceTwo
    b fis' b \change Staff = "two" fis'
    \musicalIssue \with {
      author = "Urs Liska"
      message = "sic[!] a'."
      footnote-offset = #'(.5 . 5.5)
    } NoteHead
    a fis'
  }
  \tuplet 3/2 {
    \change Staff = "one" b fis' b
  }
  \noBreak

  |
  \tuplet 6/4 4 {
    \oneVoice
    fis' b, fis b, fis \change Staff = "two" b,
    fis \change Staff = "three" b, fis b, fis b,
    fis' b fis' b \change Staff = "two" fis' b
    % End of multi-segment slur
    % TODO: Make it work with broken slurs
    )

    |
    \change Staff = "one"
    fis' b fis' b fis' b,
    fis [ b, fis ] \change Staff = "two" b, fis \change Staff = "three" b,
    \voiceTwo
    fis b, fis b, fis b
    fis' b fis' b \change Staff = "two" fis' b
    \change Staff = "one"
    \voiceOne
    fis' b fis' b fis' b,

    |
    fis b, fis \change Staff = "two" b, fis \change Staff = "three" b,
    \oneVoice
    fis b, fis b fis' b
  }

}


\score {
  \new PianoStaff <<
    \new Staff = "one" <<
      \new Voice {
        \clef "treble^8"
        \structure
      }
    >>

    \new Staff = "two" <<
      \new Voice \structure
      \new Voice {
        \voiceOne
        \voiceI
      }
      \new Voice \voiceII
    >>

    \new Staff = "three" <<
      \new Voice {
        \clef bass
        \structure
      }
      \new Voice \voiceIII
    >>
  >>
}

