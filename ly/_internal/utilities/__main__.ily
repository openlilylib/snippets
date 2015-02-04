% Make general openLilyLib utilities available to any library.
% See TODO: DOC for more information
% This file is part of the openLilyLib library infrastructure
% ... TOBEDONE ...
%
% This file initializes openLilyLib

#(ly:set-option 'relative-includes #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

% A collection of general-purpose predicates
\include "general-predicates.ily"

% Version predicates to execute code for specific LilyPond versions
\include "lilypond-version-predicates.ily"

% Helpers for handling Scheme association lists
\include "alist-access.ily"

% Advance path handling
\include "os-path.ily"
