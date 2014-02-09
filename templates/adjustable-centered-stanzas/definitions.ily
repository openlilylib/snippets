\version "2.16.2"

\header {
  snippet-title = "Adjustable centered stanzas"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
    When typesetting songs, you usually want to put additional stanzas
    below the music.
  }
  % add comma-separated tags to make searching more effective:
  tags = "stanza, stanzas, song, vocal music, centered, markup, fill-line"
  % is this snippet ready?  See meta/status-values.md
  status = "undocumented"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  TODO:
  it would be good to turn this into a function.
  the function would take the number of columns to be used as an argument
  (i.e. does the user want to have all stanzas in one column, below each other,
  or should they be in two or more columns?)
  QUESTION for that: is there a converntion how the stanzas should be ordered?
  I.e. if we have 9 stanzas in 3x3 layout, are they placed like this
  1 4 7
  2 5 8
  3 6 9
  or like this
  1 2 3
  4 5 6
  7 8 9
  ?
  We could also have a parameter that would allow to choose the arrangement.

  BUG: if columns have significantly differrent width, alignment is wrong.
%}

stanzas = \markup {
  \fill-line {
    % We don't use \large, \small etc. because these commands
    % don't scale the distance between lines correctly.
    % First number - horizontal factor, 2nd - vertical.
    \scale #'(1 . 1) {
      % without this \null, \fill-line would place the left
      % column flush with the left edge of printable area.
      \null

      % Adjust the distace between text lines (for cramped spacing).
      % 3 is the default value (it's independent from font size)
      \override #'(baseline-skip . 3)
      \column {
        \line {
          \bold
          "2."
          \spaceAfterNumber
          \stanzaII
        }
        \spaceBetweenStanzas
        \line {
          \bold
          "3."
          \spaceAfterNumber
          \stanzaIII
        }
        \spaceBetweenStanzas
      }

      \null

      \override #'(baseline-skip . 3)
      \column {
        \line {
          \bold
          "4."
          \spaceAfterNumber
          \stanzaIV
        }
        \spaceBetweenStanzas
        \line {
          \bold
          "5."
          \spaceAfterNumber
          \stanzaV
        }
        \spaceBetweenStanzas
      }

      \null
    }
  }
}
