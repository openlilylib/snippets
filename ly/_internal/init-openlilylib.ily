% Make general openLilyLib utilities available to any library.
% See TODO: DOC for more information
% This file is part of the openLilyLib library infrastructure
% ... TOBEDONE ...
%
% This file initializes openLilyLib

#(ly:set-option 'relative-includes #t)

\include "logging.ily"

% Set variables for root path and Scheme module path
\include "root-path.ily"

\include "module-handling.ily"


\loadModule "_internal/utilities/lilypond-version-predicates.ily"

