\version "2.16.2"

%{
  TODO:
  it would be good to turn this into a function.
  BUG: if columns have significantly differrent width, alignment is wrong.
%}

stanzaII = \markup {
}
stanzaIII = \markup {
}
stanzaIV = \markup {
}
stanzaV = \markup {
}

spaceAfterNumber = \markup \hspace #0.8
% it would be good if this was stretchable.
spaceBetweenStanzas = \markup \vspace #2

\markup {
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
