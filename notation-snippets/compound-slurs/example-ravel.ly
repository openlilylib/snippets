\version "2.19.48"

\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.compound-slurs
%\include "compound-slurs.ily"

\paper {
  system-count = 1
  indent = 0
  ragged-last = ##f
}

\header {
  title = "from: “Gaspard de la nuit: Ondine”"
  composer = "Maurice Ravel"
}

sixtytwoCompoundSlur = ^\compoundSlur \with {
  %annotate = ##t
  %  show-original-slur = ##t
  % show-grid = ##t

  start-point = #'(0.5 . -17)
  start-angle = -15
  start-ratio = 0.4
  end-point = #'(0 . 0)
  end-angle = -90
  end-ratio = 0.04

  inflection =
  #'((X-ratio . .11)
     (Y-offset . 5)
     (angle . 35)
     (ratio-left . 0.9)
     (ratio-right . 0.2)
     )

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
  %  _(
  \sixtytwoCompoundSlur
  \change Staff = "one"
  \oneVoice
  \set tieWaitForNote = ##t
  \grace {
    % Source: http://lists.gnu.org/archive/html/lilypond-user/2017-01/msg00036.html
    \once \override Score.NoteColumn.X-offset = #3
    \once \override Beam.positions = #'(4.25 . 4.25)
    eis64 [ ~ bis ~
    \once \override Score.NoteColumn.X-offset = #0.8
    \once \override Tie.minimum-length = 2
    \once \override Tie.Y-offset = #0.25
    gis ~ ]
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
  <a! cis fis a> )
  \clef treble
}

sixtytwoVoiceIII = \relative dis {
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment 1/8)
  \omit TupletBracket
  \voiceTwo
  \tuplet 3/2 8 {
    \shape #'((0 . 1.25)(-1 . -2)(1 . -4)(0 . 4)) Slur
    dis16 ( gis,
    dis
    gis,
    dis' gis
    \oneVoice
    \omit TupletNumber
    dis'16 gis, dis gis, dis' gis
    dis'16 gis, dis gis, dis' gis )
    |
    \shape #'((0 . 0)(0 . -2)(0 . -2)(0 . 0.5)) Slur
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


\markup { \vspace #3 }

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
    \context {
      \Score
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 16))
    }
  }
}

