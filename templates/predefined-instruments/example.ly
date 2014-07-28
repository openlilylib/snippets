\version "2.19.1"

\include "instrument-context-definitions.ily"

sopranomelody = \relative c'' {
  c b a f
}
altomelody = \relative f' {
  f g a c,
}
tenormelody = \relative c' {
  c a c c
}
bassmelody = \relative f {
  f e d c
}
text = \lyricmode {
  la la la la
}

\score {
  \new ChoirStaff <<
    \new SopranoVoice = sop \sopranomelody
    % cannot use  \addlyrics \text  because it would result
    % in ordinary Voice being created (instead of SopranoVoice)
    \new Lyrics \lyricsto sop \text

    \new AltoVoice = alt \altomelody
    \new Lyrics \lyricsto alt \text

    \new TenorVoice = ten \tenormelody
    \new Lyrics \lyricsto ten \text

    \new BassVoice = bas \bassmelody
    \new Lyrics \lyricsto bas \text
  >>
  \layout {
    \override Staff.NoteHead #'color = #blue
    \override AltoStaff.NoteHead #'color = #red
    \override SopranoVoice.Stem #'color = #green
    \override VocalStaff.Stem #'thickness = #4
  }
  \midi {
    % it would be nice if eg. \set SopranoStaff.midiInstrument
    % affected all voices living in SopranoStaves
    \set Voice.midiInstrument = #"acoustic grand"
    \set SopranoVoice.midiInstrument = #"clarinet"
  }
}
