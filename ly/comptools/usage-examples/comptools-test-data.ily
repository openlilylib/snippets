
\version "2.18.0"

% Consider openLilyLib to be included already.

% Define a list with original page breaks (barnumbers)
% Entries can also be a list with barnumber and fraction
% Also available: line-breaks and page-turns
\setOption comptools.page-breaks #'(20 37 52 66 83 91)

music = \relative c' {
  \time 4/2
  \repeat unfold 100 {
    c2 d \bar "" e4 d8 c d4 e
  }
}
