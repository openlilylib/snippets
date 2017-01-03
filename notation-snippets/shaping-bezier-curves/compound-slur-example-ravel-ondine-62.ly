\version "2.19.48"

\include "compound-slurs.ily"


\paper {
  system-count = 1
  indent = 0
  ragged-last = ##f
}

\header {
  title = "from: “Gaspard de la nuit: Ondine”"
  composer = "Maurice Ravel"
}

sixtytwoCommons = {
  \key fis \major
  \time 3/4
}

sixtytwoVoiceI = \relative dis'' {
  <<
    {
      \sixtytwoCommons
      \set Score.currentBarNumber = 62
      \set Score.barNumberVisibility = #all-bar-numbers-visible
      \bar ""
      dis8 ->
    } \\
    {
      \sixtytwoCommons
      \tuplet 6/4 {
        dis32 [ ( gis, dis
        \clef bass
        dis gis, dis ] )
      }
    }
  >>
  s8 s2 |
  s2.

}

sixtytwoVoiceII = \relative dis {
  \change Staff = "two"
  \voiceOne
  dis8 ->
  \change Staff = "one"
  \oneVoice
  \set tieWaitForNote = ##t
  \grace {
    eis64 [ ~ bis ~ gis ~ ]
  }
  \set tieWaitForNote = ##f
  <eis gis bis eis>8 [
  <fisis bis fisis'> ~
  <gis bis gis'>
  <ais gis' ais> ~
  <bis gis' bis> ] |
  <cis fis a cis>8 ~
  <dis fis a dis>
  <eis a cis eis> ~
  <fis a cis fis>
  <gis cis ~ fis _~ gis>
  <a! cis fis a>
  \clef treble
}

sixtytwoVoiceIII = \relative dis {
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment 1/8)
  \omit TupletBracket
  \voiceTwo
  \tuplet 3/2 8 {
    dis16 ( gis, dis gis, dis' gis
    \oneVoice
    \omit TupletNumber
    dis'16 gis, dis gis, dis' gis
    dis'16 gis, dis gis, dis' gis )
    |
    fis,16 ( cis' fis cis' fis, cis
    fis,16 cis' fis cis' fis, cis
    fis,16 cis' fis cis' fis, cis )
  }
}

sixtytwoDynamics = {
  \override TextSpanner.bound-details.left.text = "augmentez peu à peu"
  s8 \f \> s8 \p s2 \startTextSpan
  s2 s8. s16 \stopTextSpan
}


\score {
  \new PianoStaff <<
    \new Staff = "one" <<
      \new Voice \sixtytwoVoiceI
      \new Voice \sixtytwoVoiceII
    >>

    \new Dynamics \sixtytwoDynamics

    \new Staff = "two" <<
      \new Voice {
        \sixtytwoCommons
        \clef bass
        \sixtytwoVoiceIII
      }
    >>

  >>

  \layout {
    \context {
      \Staff
        \remove "Time_signature_engraver"

    }
  }
}

