\version "2.19.48"

\include "compound-slurs.ily"

\include "scholarly/package.ily"
\loadModule scholarly.annotate

#(set-default-paper-size "a3" 'landscape)

\paper {
  indent = 0
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
  start-angle = 65
  start-ratio = 0.6
  end-point = #'(2.5 . 4)
  end-angle = -70

  inflection =
  #'((point . (.215 . 1.55))
     (angle . -55)
     (ratio-left . 0.4)
     (ratio-right . 0.22))

  inflection =
  #'((point . (.26 . -0.2))
     (angle . 60)
     (ratio-left . .45)
     (ratio-right . .4))

  inflection =
  #'((point . (.49 . 0.35))
     (angle . 33)
     (ratio-left . .45)
     (ratio-right . .15))

  inflection =
  #'((point . (.7 . 1.6))
     (angle . -45)
     (ratio-left . .8)
     (ratio-right . .57))

  inflection =
  #'((point . (.75 . -0.05))
     (angle . 45)
     (ratio-left . .45)
     (ratio-right . .25))

}

fancySlur = _\compoundSlur \with {
%  annotate = ##t
%  show-grid = ##t

  start-point = #'(0 . 5)
  start-angle = -45
  end-angle = -125
  end-ratio = 1

  inflection =
  #'((point . (0.1 . 1.5))
     (angle . 125))

  inflection =
  #'((point . (-.3 . -6))
     (angle . 65)
     (ratio-left . .45)
     (ratio-right . .25))

  inflection =
  #'((point . (0.2 . -12))
     (angle . 45)
     (ratio-left . .45)
     (ratio-right . .25))

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
  b4 ~
  \fancySlur
  b64 c
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

