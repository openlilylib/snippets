\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "SATB choir on 2 or 4 staves"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "choir, SATB"
  % is this snippet ready?  See meta/status-values.md
  status = "undocumented"
  % TODO: convert to includable version
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.16.2"
#(set-global-staff-size 18)

\header	{
  title = ""
  composer = ""
}

%-------------------------------- MUSICAL CONTENT:

TimeKeyEtc = {
  \key d \major
}

sopranomelody = \relative f' {
  \TimeKeyEtc
  d e a fis
}
altomelody = \relative f' {
  \TimeKeyEtc
  d d d d
}
tenormelody = \relative f {
  \TimeKeyEtc
  a a fis a
}
bassmelody = \relative f {
  \TimeKeyEtc
  d d d d
}

words = \lyricmode {
  \set stanza = "1."
  la la la la
}
sopranowords = \words
altowords = \words
tenorwords = \words
basswords = \words

%-------------------------------- SCORE STRUCTURE:

\markup { on four staves: }

\score {
  \new ChoirStaff <<
    \new Staff = soprano {
      \clef treble
      \new Voice = soprano {
        \sopranomelody
      }
    }
    \new Lyrics = sopranolyrics \lyricsto soprano \sopranowords

    \new Staff = alto {
      \clef treble
      \new Voice = alto {
        \altomelody
      }
    }
    \new Lyrics = altolyrics \lyricsto alto \altowords

    \new Staff = tenor {
      \clef "treble_8"
      \new Voice = tenor {
        \tenormelody
      }
    }
    \new Lyrics = tenorlyrics \lyricsto tenor \tenorwords

    \new Staff = bass {
      \clef bass
      \new Voice = bass {
        \bassmelody
      }
    }
    \new Lyrics = basslyrics \lyricsto bass \basswords
  >>
  \layout {
  }
  \midi {
  }
}

\markup { on two staves: }

\score {
  \new ChoirStaff <<
    \new Staff = women <<
      \clef treble
      \new Voice = soprano {
        \voiceOne
        \sopranomelody
      }
      \new Voice = alto {
        \voiceTwo
        \altomelody
      }
    >>
    \new Lyrics \lyricsto soprano \words

    \new Staff = men <<
      \clef bass
      \new Voice = tenor {
        \voiceOne
        \tenormelody
      }
      \new Voice = bass {
        \voiceTwo
        \bassmelody
      }
    >>
  >>
  \layout {
  }
  \midi {
  }
}
