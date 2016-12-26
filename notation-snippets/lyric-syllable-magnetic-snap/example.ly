\version "2.18.0"

\include "./definitions.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

<<
  \new Voice = "foo" \relative c' {
    \repeat unfold 16 { a8 b a2 a8 b }
  }
  \new Lyrics \lyricsto "foo" {
    \override Lyrics.LyricWord.after-line-breaking = #(lyric-word-compressor 0)
    \override Lyrics.LyricHyphen.minimum-distance = #0
    \override Lyrics.LyricSpace.minimum-distance = #1
    \repeat unfold 3 { foof -- ifi }
    \repeat unfold 2 { AMA -- VIS }
    \repeat unfold 10 { foo -- \markup \caps bar }
    \repeat unfold 10 { \markup \bold syl -- la -- ble }
    a \markup \with-color #red ran -- \markup \box dom string of mo -- no -- syl -- la -- bic
    and mul -- ti -- \markup \fontsize #5 syl -- la -- bic
    \markup \bold \underline ver -- \markup \italic bi -- age
    \markup {
      \stencil #(make-circle-stencil 0.5 0 #f)
    }
  }
>>

<<
  \new Voice = "foo" \relative c' {
    \repeat unfold 16 { a8 b a2 a8 b }
  }
  \new Lyrics \lyricsto "foo" {
    \override Lyrics.LyricWord.after-line-breaking = #(lyric-word-compressor 0.4)
    \override Lyrics.LyricHyphen.minimum-distance = #0
    \override Lyrics.LyricSpace.minimum-distance = #1
    \repeat unfold 3 { foof -- ifi }
    \repeat unfold 2 { AMA -- VIS }
    \repeat unfold 10 { foo -- \markup \caps bar }
    \repeat unfold 10 { \markup \bold syl -- la -- ble }
    a ran -- \markup \box dom string of
    \wordunderline mo -- no -- syl -- \markup \underline la -- bic
    and \wordbox mul -- ti -- \markup \fontsize #5 syl -- la -- \markup \box bic
    \wordunderline ver -- \markup \italic bi -- age
    \markup {
      \stencil #(make-circle-stencil 0.5 0 #f)
    }
  }
>>

\layout {
  ragged-last = ##t
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
  \context {
    \Lyrics
    \consists \collectlyricwordEngraver
  }
}