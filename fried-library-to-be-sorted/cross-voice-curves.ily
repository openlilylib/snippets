\version "2.17.3"

% TODO: add oll/snippets headers, example.  use \omit?

% This looks like just an alias for hideNotes
% But it is used especially for use in tying/slurring between voices
% In addition to hideNotes it removes everything except 
% the notehead completely (namely the Flag which often
% disturbs this construct)
% and is \once
% Furthermore it suppresses a warning about clashing note columns
% as this will surely surely the case when using this shorthand for cross voice curves
hideVoiceForTie = {
  \once \override NoteHead #'transparent = ##t
  \once \override Stem #'stencil = ##f
  \once \override Dots #'stencil = ##f
  \once \override Beam #'stencil = ##f
  \once \override Flag #'stencil = ##f
  \once \override NoteColumn #'ignore-collision = ##t
}
