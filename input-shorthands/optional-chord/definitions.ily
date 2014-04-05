\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Optional chord"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
    When writing down harmonization of a piece, sometimes there are places
    where a chord changes to some other just for a brief moment, so that playing
    it as a separate chord on the accompanying instrument is optional.
    This little function is for printing such chords in smaller font and parenthesized.
  }
  % add comma-separated tags to make searching more effective:
  tags = "chord, parenthesize, small"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

optionalChord =
#(define-music-function (parser location chord)(ly:music?)
   #{
     \once \override ParenthesesItem #'font-size = 0
     \once \override ChordName #'font-size = #-1
     \parenthesize
     #chord
   #})
