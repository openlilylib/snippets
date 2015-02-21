% Demonstrate the use of the clip-regions module

\version "2.18.0"

\include "openlilylib"
%\loadModule "comptools/partial-compilation.ily"
\registerOption documentation.include-file "comptools/conditional-breaks.ily"
\loadModule "_internal/doc-include/usage-example.ily"

% Define a set with original breaks (barnumbers)
% Entries can also be a list with barnumber and fraction

% Register one set and (optionally) define lists with breaks
\registerBreakSet #'conditional-breaks-example
\setConditionalBreaks conditional-breaks-example line-breaks #'(7 12 22 25 26 (29 2/4) 34 40 56)
\setConditionalBreaks conditional-breaks-example page-breaks #'(48 125)
\setConditionalBreaks conditional-breaks-example page-turns #'(85)

% Register a second set but only define line breaks.
% The default use case is to specify breaking patterns for different manuscrips you're working with.
\registerBreakSet #'alternative-example
\setConditionalBreaks alternative-example line-breaks #'(8 15 25 (29 2/4) 32)

% Set the following options to ##f and ##t to see the different results
\setOption comptools.conditional-breaks.use.line-breaks ##t
\setOption comptools.conditional-breaks.use.page-breaks ##f
\setOption comptools.conditional-breaks.use.page-turns ##t

% Apply the breaks sets *after* all options have been set.
% Usually you will want to use only one of the break sets (i.e. comment out the other one),
% but they apply in addition, so you can also use different break sets for designing
% complex breaking structures
\applyConditionalBreaks conditional-breaks-example
%\applyConditionalBreaks alternative-example

% Some music, allowing mid-measure breaks
music = \relative c' {
  \repeat unfold 160 {
    c d \bar "" e d
  }
}

\score {
  \new Staff \music
}

