\version "2.16.2"

\include "definitions.ily"

%%%%%%%%%%%%%%%%%%%%%
%   USAGE EXAMPLE   %
%%%%%%%%%%%%%%%%%%%%%

% a shorthand for tagging:

tagIt = \once \override Lyrics.LyricText #'tagged = ##t

\layout {
  ragged-right = ##f
  \context {
    \Lyrics
    \override LyricText #'X-offset = #X-offset-callback
  }
  \context {
    \Score
    \consists #Lyric_text_align_engraver
  }
}

\score {
  \new Staff <<
    \new Voice = A {
      \relative c' {
        c d e2 \bar "|."
      }
    }
    \new Lyrics \lyricsto A {
      \tagIt Do -- mi -- nus,
    }

    \new Lyrics \lyricsto A {
      Cant -- a me
    }
    \new Lyrics \lyricsto A {
      Syll -- a -- bum!
    }
  >>
}
