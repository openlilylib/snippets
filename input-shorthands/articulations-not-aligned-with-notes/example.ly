\version "2.17.25"

\include "./definitions.ily"

% examples:

\paper {
  line-width = 9 \cm
  indent = 0
  ragged-right = ##f
}

\markup "You can insert an articulation in the middle of the note:"
\new Voice \relative c'' {
  \at 4 \turn f4. g16-. a-.
}

\markup "You can add (multiple) dynamics in the middle of the note:"
\new Voice {
  \at 1*1/3 \p
  \at 1*2/3 \<
  \at 1\!
  c'1
}

\markup "You can also apply this to a whole music expression:"
\new Voice {
  \at 4 \< \at 2. \! \at 1 \p { c'2 d' e' }
}

\markup "You can even insert music into music this way:"
<<
  \new Staff { \at 2 { e'8 f' g' a' } c'1 }
  \new Staff { c'4 d' e' f' }
>>

\markup "And a really nice example by David Kastrup:"
\new Staff
{
  <<
    {
      \dynamicUp
      \at 1*1/3\p
      \at 1*2/3\<
      \at 1 \!
      c''1\mf\>
    }
    \\
    {
      <> \pp \<
      \repeat unfold 16 cis'16
      <>\mp
    }
  >>
  <es' g' cis'' e''>1
}
