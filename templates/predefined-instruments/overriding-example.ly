\version "2.19.1"

\include "instrument-context-definitions.ily"

\layout {
  \newInstrument "Test" \Staff "" \ChoirStaff {
    \set instrumentName = "Test"
    \set shortInstrumentName = "Test"
    \dynamicUp
    \tupletUp
  }
}

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
    % cannot use \addlyrics because it's dumb
    % and would create a plain Voice
    \new SopranoVoice = sop \sopranomelody
    \new Lyrics \lyricsto sop \text

    \new TestStaff = trolo \sopranomelody

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
    \override TestStaff.Stem #'color = #magenta
  }
  \midi {
    % it would be nice if eg. \set SopranoStaff.midiInstrument
    % affected all voices living in SopranoStaves
    \set Voice.midiInstrument = #"acoustic grand"
    \set SopranoVoice.midiInstrument = #"clarinet"
  }
}
