% Formats for example pages in openlilylib documentation

#(ly:set-option 'relative-includes #t)

% Consistently format and process headers and footers,
% populated with data from the snippet's file
\include "titling.ily"

% This is still optional. I thought of providing a certain binding margin.
\paper {
  left-margin = 2\cm
  right-margin = 1.5\cm
}
