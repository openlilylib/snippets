% Demonstrate the use of the clip-regions module

\version "2.18.0"

\include "openlilylib"
%\loadModule "comptools/partial-compilation.ily"
\registerOption documentation.include-file "comptools/partial-compilation.ily"
\loadModule "_internal/doc-include/usage-example.ily"

% Define a list with original page breaks (barnumbers)
% Entries can also be a list with barnumber and fraction
% Also available: line-breaks and page-turns
\setOption comptools.page-breaks #'(112 224 336 448 560 672 784)

% Uncomment the following commands to test different partial regions.
% Multiple (non-overlapping) regions can be set, although the results
% may not be acceptable.

% Define a region with barnumbers
%\setClipRegion 8 12

% Define a region beyond measure borders
%\setClipRegion 198 #'(212 2/4)

% Compile a single page
%\setClipPage 5

% Compile a page range
%\setClipPageRange 4 5

% Negative page range triggers a warning
%\setClipPageRange 5 3

% Non-existent pages result in errors
%\setClipPageRange 3 123
%\setClipPage -2

music = \relative c' {
  \repeat unfold 800 {
    c d e d
  }
}

\score {
  \new Staff \music
}

