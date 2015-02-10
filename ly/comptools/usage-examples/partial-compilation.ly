% Demonstrate the use of the clip-regions module

\version "2.18.0"

\include "openlilylib"
\loadModule "comptools/partial-compilation.ily"

% Define a list with original page breaks (barnumbers)
conditionalPageBreaks = #'(112 224 336 448 560 672 784)

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

