\version "2.16.2"

\header {
  snippet-title = "Voice colors"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
    This snippet redefines some commonly used commands like
    "\voiceOne", "\dynamicUp" etc. so that the grobs modified
    with them are colored.  Meant to be used in Frescobaldi's
    “preview” mode.
  }
  % add comma-separated tags to make searching more effective:
  tags = "color, preview, frescobaldi"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define voiceOneColor '(0 0.5 0))
#(define voiceTwoColor '(0 0 0.5))
#(define directionUpColor '(0 0.9 0))
#(define directionDownColor '(0 0 0.9))

voiceOne = {
  \voiceOne
  \override NoteHead #'color = #voiceOneColor
  \override Stem #'color = #voiceOneColor
  \override Beam #'color = #voiceOneColor
  \override Slur #'color = #voiceOneColor
  \override Tie #'color = #voiceOneColor
}

voiceTwo = {
  \voiceTwo
  \override NoteHead #'color = #voiceTwoColor
  \override Stem #'color = #voiceTwoColor
  \override Beam #'color = #voiceTwoColor
  \override Slur #'color = #voiceTwoColor
  \override Tie #'color = #voiceTwoColor
}

oneVoice = {
  \oneVoice
  \revert NoteHead #'color
  \revert Stem #'color
  \revert Beam #'color
  \revert Slur #'color
  \revert Tie #'color
}



dynamicUp = {
  \dynamicUp
  \override DynamicText #'color = #directionUpColor
  \override DynamicLineSpanner #'color = #directionUpColor
  \override DynamicTextSpanner #'color = #directionUpColor
  \override Hairpin #'color = #directionUpColor
}

dynamicDown = {
  \dynamicDown
  \override DynamicText #'color = #directionDownColor
  \override DynamicLineSpanner #'color = #directionDownColor
  \override DynamicTextSpanner #'color = #directionDownColor
  \override Hairpin #'color = #directionDownColor
}

dynamicNeutral = {
  \dynamicNeutral
  \revert DynamicText #'color
  \revert DynamicLineSpanner #'color
  \revert DynamicTextSpanner #'color
  \revert Hairpin #'color
}



slurUp = {
  \slurUp
  \override Slur #'color = #directionUpColor
  \override PhrasingSlur #'color = #directionUpColor
}

slurDown = {
  \slurDown
  \override Slur #'color = #directionDownColor
  \override PhrasingSlur #'color = #directionDownColor
}

slurNeutral = {
  \slurNeutral
  \revert Slur #'color
  \revert PhrasingSlur #'color
}



stemUp = {
  \stemUp
  \override Stem #'color = #directionUpColor
}

stemDown = {
  \stemDown
  \override Stem #'color = #directionDownColor
}

stemNeutral = {
  \stemNeutral
  \revert Stem #'color
}