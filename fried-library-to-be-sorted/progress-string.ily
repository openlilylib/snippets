\version "2.17.3"
%{PROJECT:
  This file belongs to the project
  "Oskar Fried. Complete Lieder"
  edited from the original prints by Urs Liska and Alexander Gurdon (mail@ursliska.de, alexander.gurdon@tu-dortmund.de)
%}

%{TYPE:
  This file is part of the global options
%}

%{FILE:
  After the refactoring of the includes to ulLibrary
  and changing the include strategy, the only use
  for this file is filling the progressString
%}

% String to be added in the first-page header
progressString = \markup {
  \override #'(font-name . "Cronos Pro")
  \rounded-box \concat {
    "Edition finished: "
    \fromproperty #'header:editionfinished
    " &ndash; Print ready: "
    \fromproperty #'header:printready
    " &ndash; LilyPond " #(lilypond-version)
  }
}
