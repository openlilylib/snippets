\version "2.16.2"

\include "aligning-first-lyric-syllables.ily"

\header {
  title = "Aligning first lyric syllables in each system together"
  composer = "David Nalesnik"
}

\markup \vspace #2 
  
\markup {
    \wordwrap {
       This snippet allows you to left-align selected columns of syllables in vocal
       music with multiple verses.  The column will be positioned so that the longest
       syllable is centered on the note.  This is particularly useful at the beginnings
       of lines.  To request the alignment at a particular location, you need to
       "/"mark/"" one of the syllables there.  This snippet is also a demonstration of
       defining and using a tag of sorts, here to control a Scheme engraver.
    }
    
  }
\markup \vspace #2

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
