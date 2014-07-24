\version "2.18.2"
\include "english.ly"
#(set-global-staff-size 18)

\include "font-register.ily"

\paper {
  #(set-paper-size "letter")
  indent = 1\in
  top-margin = 0.5\in
  bottom-margin = 0.5\in
  left-margin = 0.5\in
  right-margin = 0.5\in
  ragged-bottom = ##f
  ragged-last-bottom = ##f
}

\header {
  title = "Font Sampler Sheet"
  subtitle = "(Nonsense Music)"
  composer = \markup \italic "By: Abraham Lee"
  copyright = "Copyright (c) 2014 by Abraham Lee"
}

foobar = {
  \new PianoStaff <<
    \new Staff {
      \clef treble
      \key a \major
      \time 4/4
      r4
      <<
        \relative c' {
          <e a css>2.\sfz (
          \key ef \major
          \bar "||"
          <ef a c>1\pp \startTrillSpan
          \time 3/4
          b'4 \stopTrillSpan \slashedGrace d16 c4._> b8 \mp
          f2\fff e16_.\fermata ) bf'\rest bf8\rest
        }
        \\
        \relative c' {
          s2.
          s1
          d2.
          d2.
        }
      >>
    }
    \new Staff {
      \autoBeamOff
      \clef bass
      \key a \major
      \time 4/4
      r4
      \relative c' {
        a2.-\flageolet ~ _( \sustainOn
        \key ef \major
        <ef\harmonic a>1 -\prallprall
        \time 3/4
        \arpeggioArrowUp <fs, bf fs'! c'>2.-^\arpeggio
        <eff' g b?>8) \sustainOff <d f>32 r32 r16 r2
      }
    }
  >>
  \bar "|."
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = \markup \center-column { "Emmentaler" "(Default)" }
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Amadeus"
    #(define font-defaults
       '((font-family . amadeus) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'amadeus
    \override Staff.TrillSpanner.font-family = #'amadeus
    \override Staff.DynamicText.font-family = #'amadeus    
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Cadence"
    #(define font-defaults
       '((font-family . cadence) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'cadence
    \override Staff.TrillSpanner.font-family = #'cadence
    \override Staff.DynamicText.font-family = #'cadence
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Gonville"
    #(define font-defaults
       '((font-family . gonville) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'gonville
    \override Staff.TrillSpanner.font-family = #'gonville
    \override Staff.DynamicText.font-family = #'gonville
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Gutenberg1939"
    #(define font-defaults
       '((font-family . gutenberg) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'gutenberg
    \override Staff.TrillSpanner.font-family = #'gutenberg
    \override Staff.DynamicText.font-family = #'gutenberg
  }
}

\pageBreak

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Haydn"
    #(define font-defaults
       '((font-family . haydn) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'haydn
    \override Staff.TrillSpanner.font-family = #'haydn
    \override Staff.DynamicText.font-family = #'haydn
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "LilyJAZZ"
    #(define font-defaults
       '((font-family . lilyjazz) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'lilyjazz
    \override Staff.TrillSpanner.font-family = #'lilyjazz
    \override Staff.DynamicText.font-family = #'lilyjazz
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Paganini"
    #(define font-defaults
       '((font-family . paganini) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'paganini
    \override Staff.TrillSpanner.font-family = #'paganini
    \override Staff.DynamicText.font-family = #'paganini
  }
}

\score {
  { \foobar }
  \layout {
    \set PianoStaff.instrumentName = "Profondo"
    #(define font-defaults
       '((font-family . profondo) (font-encoding . fetaMusic)))
    \override Staff.TimeSignature.font-family = #'profondo
    \override Staff.TrillSpanner.font-family = #'profondo
    \override Staff.DynamicText.font-family = #'profondo
  }
}

