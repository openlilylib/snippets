
\version "2.18.0"

% Consider openLilyLib to be included already.

% Define a list with original page breaks (barnumbers)
% Entries can also be a list with barnumber and fraction
% Also available: line-breaks and page-turns
\setOption comptools.page-breaks #'(112 224 336 448 490 560 672 784)

music = \relative c' {
  \repeat unfold 800 {
    c d \bar "" e d
  }
}
