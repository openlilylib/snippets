\version "2.17.3"
% /includes/ulLibrary/curvesToolbox/crossVoiceCurves.ily

%{
  This library is part of ulLibrary (Urs Liska's LilyPond Toolbox)
  and may also be included separately.
  It assumes that the project root contains any number of
  first level subdirectories containing the to-be-compiled .ly files
  and a folder /includes/ulLibrary.
  So this file is always accessible through the relative path "../includes/ulLibrary/curvesToolbox/crossVoiceCurves.ily

  crossVoiceCurves contains
  tools that make it easier to implement curves that cross voices,
  mainly a function that implements the hidden note with a single command.
  The corresponding draft mode also highlights this hidden note
  (because it is sometimes hard to find hidden notes, e.g. when they are causing
  clashing NoteColumns)

  This file is complemented by crossVoiceCurves_draftMode.ily
%}



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

% highlight CrossVoiceCurve
% can be used to make ties, slurs and phrasingSlurs visible 
% that are use for voice crossings
% As it isn't possible to make hiddenNotes visible in draftMode
% this is a workaround (but needs this to be included
% before the curve
% 
% As it now seems to be possible to directly highlight hideVoiceForTie
% This function isn't necessary anymore but can optionally be used to
% also highlight the connecting element (curve, StaffSwitch)
% 
% Here it is only a stub to provide consisten input syntax
% the coloring is done the corresponding Draft Mode file
hiCVC = {}

