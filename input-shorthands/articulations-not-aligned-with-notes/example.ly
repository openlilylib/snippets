\version "2.17.25"

\include "./definitions.ily"

% examples:

\paper {
  line-width = 7 \cm
  indent = 0
  ragged-right = ##f
}

\new Voice {
  \at 1*1/3 \p
  \at 1*2/3 \<
  \at 1\!
  c'1
}

\new Voice {
  \at 4. \< \at 2 \! { c'4 d' e' }
}

<<
  \new Staff { \musat 2 { e'8 f' g' a' } c'1 }
  \new Staff { c'4 d' e' f' }
>>

\new Voice \relative c'' {
  \at 4 \turn f4. g16-. a-.
}

% (BUG?) why this produces different output than the above code?
\new Voice \relative c'' {
  << f4. {s4 \once\hideNotes f8\turn\noBeam } >> g16-. a-.
}
