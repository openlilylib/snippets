\version "2.19.10"

% Create a new Staff-like and Voice-like contexts with specified settings,
% both in \layout and \midi output-definitions.  Unfortunately, I didn't
% manage to get this function accept ly:context-def? arguments specifying
% parent contexts from which settings could be overridden...
newInstrument =
#(define-void-function
  (parser location name settings)
  (string? ly:context-mod?)
  (let ((staffname (string-append name "Staff"))
        (voicename (string-append name "Voice")))
    ;; We have to make a Scheme assignment to make these work as output-def modifications.
    ;; From David's explanation (http://lists.gnu.org/archive/html/lilypond-user/2014-01/msg00457.html):

    ;; [That's] Presumably because Scheme expressions in layout definitions are not
    ;; interpreted.  That's similar to a few other places where a local module
    ;; can be manipulated by Scheme expressions like #(set-staff-size) and
    ;; similar stuff where the expectation is that the return value should not
    ;; get interpreted.
    (ly:parser-define! parser '$defaultlayout
      #{
        \layout {
          \context {
            \ChoirStaff
            \accepts #staffname
          }
          \context {
            \Staff
            \name #staffname
            \alias "Staff"
            \accepts #voicename
            \defaultchild #voicename
            #settings
          }
          \context {
            \Voice
            \name #voicename
            \alias "Voice"
          }
        }
      #})
    ;; code duplication, UGH!!!
    (ly:parser-define! parser '$defaultmidi
      #{
        \midi {
          \context {
            \ChoirStaff
            \accepts #staffname
          }
          \context {
            \Staff
            \name #staffname
            \alias "Staff"
            \accepts #voicename
            \defaultchild #voicename
            #settings
          }
          \context {
            \Voice
            \name #voicename
            \alias "Voice"
          }
        }
      #})))


% define "soprano" contexts:

\newInstrument "Soprano"
\with {
  \consists "Ambitus_engraver"
  instrumentName = "Soprano"
  shortInstrumentName = "S"
  \dynamicUp
  \tupletUp
  \clef G
  midiInstrument = "choir aahs"
}


% test:

\score {
  \new SopranoVoice \relative f' {
    c f c' f
  }
  \layout {
    \override Voice.Stem.color = #blue
    \override SopranoVoice.NoteHead.color = #green
  }
  \midi {
    \set SopranoStaff.midiInstrument = "clarinet"
  }
}
