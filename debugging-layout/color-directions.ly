\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Color Directions"
  snippet-author = "Urs Liska, Janek Warchoł"
  snippet-description = \markup {
    Color objects whose direction has been set with
    the predefined shorthands.
  
  }
  % add comma-separated tags to make searching more effective:
  tags = ""
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
}
%{
  This file is still a sketch.
  
  TODO:
  - Can we include David Kastrup's \colorizeDir here?
  - Add ties
  - More grob types?
    (both through command redefinition and \colorizeDir)
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define appearance
#(cond ((not (defined? 'debug-direction-up-color))
        (define debug-direction-up-color blue)))
#(cond ((not (defined? 'debug-direction-down-color))
        (define debug-direction-down-color blue)))



dynamicUp = {
  \dynamicUp
  \override DynamicText #'color = #debug-direction-up-color
  \override DynamicLineSpanner #'color = #debug-direction-up-color
  \override DynamicTextSpanner #'color = #debug-direction-up-color
  \override Hairpin #'color = #debug-direction-up-color
}

dynamicDown = {
  \dynamicDown
  \override DynamicText #'color = #debug-direction-down-color
  \override DynamicLineSpanner #'color = #debug-direction-down-color
  \override DynamicTextSpanner #'color = #debug-direction-down-color
  \override Hairpin #'color = #debug-direction-down-color
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
  \override Slur #'color = #debug-direction-up-color
}

slurDown = {
  \slurDown
  \override Slur #'color = #debug-direction-down-color
}

slurNeutral = {
  \slurNeutral
  \revert Slur #'color
}

phrasingSlurUp = {
  \phrasingSlurUp
  \override PhrasingSlur #'color = #debug-direction-up-color
}

phrasingSlurDown = {
  \phrasingSlurDown
  \override PhrasingSlur #'color = #debug-direction-down-color
}

phrasingSlurNeutral = {
  \phrasingSlurNeutral
  \revert PhrasingSlur #'color
}


stemUp = {
  \stemUp
  \override Stem #'color = #debug-direction-up-color
  \override NoteHead #'color = #debug-direction-up-color
  \override Flag #'color = #debug-direction-up-color
  \override Beam #'color = #debug-direction-up-color
}

stemDown = {
  \stemDown
  \override Stem #'color = #debug-direction-down-color
  \override NoteHead #'color = #debug-direction-down-color
  \override Flag #'color = #debug-direction-down-color
  \override Beam #'color = #debug-direction-down-color
}

stemNeutral = {
  \stemNeutral
  \revert Stem #'color
  \revert NoteHead #'color
  \revert Flag #'color
  \revert Beam #'color
}

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%
{
  \stemUp g'4 a' \stemNeutral \slurUp b'( a')
}
