\version "2.19.49"

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
    "Adding slashes to stems"
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
  \fill-line { \italic "October 2016" }
}

\markup \vspace #2

\markup \justify {

  It is sometimes necessary to add slashes of various types to note
  stems, not just in the context of grace notes. While there is code
  available to make slashes through stems, none presently offers the
  capability of specifying various parameters that the present code
  does.  The function \typewriter "\slashStem" described here offers a
  flexible beam slash capability for lilypond, and allows fine tuning
  of the slash geometry and line properties.

}

\markup \vspace #2
\markup \large \bold "Usage"
\markup \vspace #1

\markuplist {
  \justified-lines {

    The function is a normal lilypond music function taking several
    arguments and is called before the notes you want to modify. The
    function adds the specified slash to each following note, until
    explicitly turned off.

  }
  \vspace #1
  \justified-lines {

    \typewriter "\slashStem <length> <fraction> <thickness> <slash-angle>"

  }
  \vspace #0.5
  \line {
    where
  }
  \vspace #1
}

\markup {
  %\vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \justify {

    \typewriter "length" — length of slash line, in staff spaces.

  }
}

\markup \vspace #0.5

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \justify {

    \typewriter "fraction" — intersection point of slash with stem,
    as a fraction of stem length. A number between \typewriter "0" and \typewriter "1.0".

  }
}

\markup \vspace #0.5

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \justify {

    \typewriter "thickness" — thickness of the slash line.

  }
}


\markup \vspace #0.5

\markup {
  \vspace #0.5
  \hspace #5
  \override #'(line-width . 70)

  \justify {

    \typewriter "slash-angle" — angle of slash with respect to the
    horizontal, or x-axis, in degrees. Can be negative.

  }
}

\markup \vspace #2
\markup \large \bold "Examples"
\markup \vspace #1

\markup {
  \justify {

    Some examples of the usage follow. Because this function is
    designed to offer a lot of flexibility it does not simply rely on a
    standard set of defaults that cover every case. In general, each
    slash needs to be tuned to look right in the given context. This
    is why the various parameters must be specified for each call.

  }
}

\pageBreak

\markup {
  \typewriter "\slashStem 1.6 0.55 0.3 45"
}

{
  \slashStem 1.6 0.55 0.3 45
  c'' a' g' c''
  c''8 a' g' c''
  b' a' b' e'
  c''16 a' g' c''
  \slashStemOff
  c''4 b' a'

}

\markup {
  \typewriter "\slashStem 2 0.5 0.8 -20"
}

{
  \slashStem 2 0.5 0.8 -20
  c'' a' g' c''
  c''8 a' g' c''
  b' a' b' e'
  c''16 a' g' c''
  \slashStemOff
  c''4 b' a'

}

\markup {
  \typewriter "\slashStem 1 0 1 0"
}

{
  \slashStem 1 0 1 0
  c'' a' g' c''
}

\markup \vspace #1
\markup\large\bold "Notes"
\markup \vspace #1

\markup {
  \justify {

    This code can clearly be generalized easily to make other marks
    apart from lines for slashes. It is simple to adapt the code to
    add crosses to stems for example, or circles, or any other
    graphical construct, or even multiple objects per stem. This code
    provides a ready made framework for that. Implementation is left
    as an exercise for the reader.

  }
}

\markup \vspace #1
\markup\large\bold "Author"
\markup \vspace #1
\markup {
  \typewriter "andrew.bernard@gmail.com"
}

