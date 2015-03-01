% Demonstrate the use of the clip-regions module

\version "2.18.0"

\include "openlilylib"
\registerOption documentation.include-file "comptools/partial-compilation.ily"
\loadModule "_internal/doc-include/usage-example.ily"

\include "comptools-test-data.ily"

% Uncomment the following commands to test different partial regions.
% Multiple (non-overlapping) regions can be set, although the results
% may not be acceptable.

% Define a region with barnumbers
%\setClipRegion 8 12

% Define a region beyond measure borders
%\setClipRegion 65 #'(87 2/2)

% Compile a single page
%\setClipPage 5

% Compile a page range
%\setClipPageRange 4 5

% Negative page range triggers a warning
%\setClipPageRange 5 3

% Non-existent pages result in errors
%\setClipPageRange 3 123
%\setClipPage -2


\score {
  \new Staff \music
}

