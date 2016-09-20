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
  show-control-points = ##t
  show-original-slur = ##t
  offsets =
  #'((0 . 3)
     (-6 . 5)
     (-20 . -75)
     (0 . 3))
  inflections =
  #'(((point . (.435 . 1.4))
      (angle . -126)
      (ratio-left . 0.4)
      (ratio-right . 0.75)))
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
  } Beam
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
  b4 ~ b64 c
  \change Staff = "two"
  d64 e b c es f
  fis128 [
  \musicalIssue \with {
    author = "Urs Liska"
    message = "Accidental is missing but seems obvious."
  }
  Accidental
  e fis16.. a8 cis ]

  |
  \tuplet 5/4 {
    e,64-> d e fis f
  }
  \musicalIssue \with {
    author = "Urs Liska"
    message = "OE prints double dotted eighth note. Either the additional dot is wrong 
or the preceding group should be 128th notes. However, horizontal alignment with lower voice
suggests our reading."
  } NoteHead
  e8.
  \musicalIssue \with {
    author = "Urs Liska"
    message = "OE prints dotted eighth note. Either the dot is wrong or the
following group should be 128th notes. However, horizontal alignment with lower voice
suggests our reading."
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
    )
    \musicalIssue \with {
      author = "Urs Liska"
      message = "sic[!] a'."
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

