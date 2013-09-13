\version "2.17.25"

\header {
  %FIXME: better name?
  title = "Articulations not aligned to notes"
  author = "David Kastrup"
  status = "undocumented"
  tags = "syntax"
}

% put the snippet here:
at =
#(define-music-function (parser location t e m)
   (ly:duration? ly:music? ly:music?)
   #{ << #m { \skip $t <>$e } >> #})

musat =
#(define-music-function (parser location t e m)
   (ly:duration? ly:music? ly:music?)
   #{ << #m { \skip $t <>$e } >> #})

% Question: would it be possible to merge above two functions?

% examples:

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
