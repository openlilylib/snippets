\version "2.16.2" % absolutely necessary!

% use this to crop output page size to match the snippet
#(ly:set-option 'preview #t)

\header {
  snippet-title = "Scaling stencils"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
    Sometimes you want to scale (stretch or squeeze) an object -
    for example, make a flag shorter so that a tie can be placed below it,
    or make a lyric syllable narrower so that it won't distort the note
    spacing.  To do this, you have to override the stencil property of
    that object.
    Note that you can place such override in a "\layout" block and thus
    make, for example, all lyrics in your piece narrower by a certain factor.
  }
  % add comma-separated tags to make searching more effective:
  tags = "scale, stretch, squeeze, lyrics, flag, stencil"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

{
  <e' g'>4.
  \once \override Flag #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (ly:flag::print grob) 1 0.8))
  q8~q8 q4.
}
\addlyrics {
  long
  \override LyricText #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (lyric-text::print grob) 0.8 1))
  long
  long
}
