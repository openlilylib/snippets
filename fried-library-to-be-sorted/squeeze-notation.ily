\version "2.16.2"

% TODO: parametrize (allow custom levels of squeezing).
% other grobs to squeeze?
% check if this function accumulates.

squeezeNotation = {
  \override Staff.AccidentalPlacement #'right-padding = #-0.05
  \override Staff.Accidental #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (ly:accidental-interface::print grob) 0.92 1))
  \override Staff.NoteHead #'stencil =
  #(lambda (grob)
     (ly:stencil-scale (ly:note-head::print grob) 0.96 1.02))
}

unsqueezeNotation = {
  \revert Staff.AccidentalPlacement #'right-padding
  \revert Staff.Accidental #'stencil
  \revert Staff.NoteHead #'stencil
}