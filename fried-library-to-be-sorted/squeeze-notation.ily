\version "2.16.2"

% TODO: add oll/snippets headers, add example.

% TODO: parametrize (allow custom levels of squeezing).
% other grobs to squeeze? e.g. flags
% check if this function accumulates.


squeezeNotation = {
  \temporary \override Staff.AccidentalPlacement #'right-padding = #-0.05
  
  % TODO: also override LedgerLineSpanner.minimum-length-fraction
  
  % TODO: should use narrow accidentals when they're available,
  % http://code.google.com/p/lilypond/issues/detail?id=2203
  % TODO: only make the override if the stencil exists
  % (currently the function fails when the stencil is empty).
  \temporary \override Staff.Accidental #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (ly:accidental-interface::print grob) 0.92 1))
  
  % TODO: design a narrow notehead glyph??
  % TODO: only make the override if the stencil exists
  % (currently the function fails when the stencil is empty).
  \temporary \override Staff.NoteHead #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (ly:note-head::print grob) 0.96 1.02))
  
  % also, change tracking between letters (there is some snippet doing this)
  % TODO: only make the override if the stencil exists
  % (currently the function fails when the stencil is empty).
  \temporary \override Lyrics.LyricText #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (lyric-text::print grob) 0.92 1))
}

%{
  just use \undo \squeezeNotation
  
unsqueezeNotation = {
  \revert Staff.AccidentalPlacement #'right-padding
  \revert Staff.Accidental #'stencil
  \revert Staff.NoteHead #'stencil
  \revert Lyrics.LyricText #'stencil
}
%}
