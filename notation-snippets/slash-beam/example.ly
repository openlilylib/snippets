\version "2.19.45"

\include "./definitions.ily"

\paper {
  ragged-right = ##t
  indent = 0
  left-margin = 25\mm
  right-margin = 25\mm
  top-margin = 20\mm
  bottom-margin = 20\mm

  top-markup-spacing =
  #'((basic-distance . 8)
     (minimum-distance . 4)
     (padding . 1)
     (stretchability . 12))
}

\header {
  title = \markup \column {
    \vspace #2
    "Adding slashes to beams"
    \vspace #1
  }
  subtitle = "A flexible slash function"
  tagline = ##f
}

\markup \vspace #2

\markup {
  \fill-line { \italic "Andrew Bernard" }
}

\markup \vspace #0.5

\markup {
  \fill-line { \italic "July 2016" }
}

\markup \vspace #2

\markup \justify {

  It is occasionally necessary to add a diagonal slash to the corner of a beamed
  group of notes. Traditional engraving uses a slash on the left hand side of a
  beamed group to indicate acciaccaturas, for example. Lilypond has limitations
  in this area, so a function is needed to provide this capability. Contemporary
  scores sometimes use slashes on the right hand side of a beamed group. The
  function \typewriter "\slashBeam" described here offers a flexible beam slash
  capability for lilypond, and allows fine tuning of the slash geometry.

}

\markup \vspace #2
\markup \large \bold "Usage"
\markup \vspace #1

\markuplist {
  \justified-lines {

    The function is a normal lilypond music function taking several arguments
    and is called before the beam you want to modify. The function only adds a
    slash to the single following beamed group. Note also that this function
    is not designed to add a slash to a single note.

  }
  \vspace #1
  \justified-lines {
    \typewriter "\slashBeam <slash-angle> <beam-fraction> <outer-proportion> <side>"
  }
  \vspace #0.5
  \line {
    where
  }
  \vspace #1
}

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \fill-line {
    \center-column {
      \justify {
        \typewriter "slash-angle" — angle of slash with respect to the horizontal, in degrees.
      }
    }
  }
}

\markup \vspace #0.5

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \fill-line {
    \center-column {
      \justify {
        \typewriter "beam-fraction" — intersection point of slash with beam, as a fraction of beam length,
        always measured from the left hand side. A number between 0 and 1.
      }
    }
  }
}

\markup \vspace #0.5

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \fill-line {
    \center-column {
      \justify {
        \typewriter "outer-proportion" — length of the outside projections as a proportion of the length of the
        inner segment between beam and stem.
      }
    }
  }
}

\markup \vspace #0.5

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \fill-line {
    \center-column {
      \justify {
        \typewriter "side" — the side of the beam to add the slash to. Either the symbol "#'left" or "#'right".
      }
    }
  }
}

\markup \vspace #2
\markup \large \bold "Examples"
\markup \vspace #1

\markup {
  \justify {

    Some examples of the usage follow. Because beams have varying slope and
    orientation the function does not simply rely on a standard set of
    defaults that cover every case. In general, each slash needs to be tuned
    to look right in the given context. This is why the various parameters
    must be specified for each call.

  }
}

\pageBreak

\markup {
  \typewriter "\slashBeam 50 0.15 0.4 #'left"
}
\markup \vspace #1

setBeamPos =
#(define-music-function (left right)
   (number? number?)
   "Manually set beam position for next group."
   #{
     \once \override Beam.positions = #(cons left right)
   #})

{
  \stemUp
  \setBeamPos 6 4
  \slashBeam 50 0.15 0.4 #'left
  a''8 [ b' bes' cis' c'] s8
}

\markup {
  \typewriter "\slashBeam 60 0.85 0.4 #'right"
}
\markup \vspace #1
{
  \stemUp
  \setBeamPos 6 4
  \slashBeam 60 0.85 0.4 #'right
  a''8 [ b' bes' cis' c'] s8
}
\markup {
  \typewriter "\slashBeam 60 0.92 0.4 #'right"
}
\markup \vspace #1
{
  \stemDown
  \setBeamPos -6 -6
  \slashBeam 60 0.92 0.4 #'right
  a''8 [ b' bes' cis' c'] s8
}

\markup {
  \typewriter " \slashBeam 45 0.92 1 #'right"
}
\markup \vspace #1
{
  \stemDown
  \setBeamPos -4 -6
  \slashBeam 45 0.92 1 #'right
  a''8 [ b' bes' cis' c'] s8
}

\markup \vspace #1
\markup\large\bold "Acknowledgements"
\markup \vspace #1
\markup \vspace #0.5
\markup \justify {

  This code is based on concepts by David Nalesnik (see LSR 721) building on
  original code by Valentin Villenave, and on suggestions from Thomas Morley,
  Abraham Lee, and Urs Liska. Thank you to all.

}

\markup \vspace #1
\markup\large\bold "TODO"
\markup \vspace #1

\markup {
  \column {
    "• Add property to override thickness."
    "• Add property to specify line style."
    "• Consider adding means to store parameters as a set of defualts."
  }
}

\markup \vspace #1
\markup\large\bold "Author"
\markup \vspace #1
\markup {
  \typewriter "andrew.bernard@gmail.com"
}

