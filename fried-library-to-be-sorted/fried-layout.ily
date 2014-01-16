\version "2.17.3"

% TODO: check if everything should be kept. add oll/snippets headers, add example.
% see how this relates to `lied-stylesheet.ily` and possibly merge them.
% update syntax to 2.18


%{FILE:
  This file specifies global context properties
  that influence all scores in the book.
  In this file the default page and score layout options
  are determined.
  This file is \included from global.ily
%}

\paper {
  %% Footnotes
  %% override definitions from paper-defaults-init.ly
  footnote-separator-markup = \markup \null
  footnote-padding = 0.5\mm
  footnote-footer-padding = 2.5\mm
  footnote-number-raise = 0.5\mm
  footnote-numbering-function = #numbered-footnotes
  reset-footnotes-on-new-page = ##t
}

\layout {
  \context {
    \Score
    % Changes to the visual appearance

    % Revert the setting from defaultLiedA4
    \override BarNumber #'font-size = #-1.5

    % Janek's addition (no real idea what it does)
    \override BarLine #'space-alist =
    #'((time-signature . (extra-space . 0.75))
       (custos . (minimum-space . 2.0))
       (clef . (minimum-space . 1.0))
       (key-signature . (extra-space . 1.0))
       (key-cancellation . (extra-space . 1.0))
       (first-note . (fixed-space . 1))
       (next-note . (minimum-fixed-space  . 1.2))
       (right-edge . (extra-space . 0.0)))

    % place key cancellations before the bar line, 
    % the new key after it
    \override BreakAlignment #'break-align-orders =
    #'#((left-edge ambitus breathing-sign clef
    key-cancellation staff-bar key-signature time-signature custos)
    (left-edge ambitus breathing-sign clef key-cancellation
    staff-bar key-signature time-signature custos)
    (left-edge ambitus breathing-sign clef key-cancellation
    key-signature staff-bar time-signature cushtos))

    % FootnoteItem is effectively only the footnote sign in the score
    % The actual footnote style is defined in the \paper section above
    \override FootnoteItem #'annotation-line = ##f
    \override FootnoteItem #'font-name = "Minion Pro"
    \override FootnoteItem #'font-size = #1.6

    %TODO: What really is a footnote spanner?
    % Font doesn't seem to make any effect here
    \override FootnoteSpanner #'font-name = "Cronos Pro Bold"
    \override FootnoteSpanner #'font-size = #-2
    \override FootnoteSpanner #'color = #red

    % Remove the repetition of the spanner text after a line break
    \override TextSpanner #'(bound-details left-broken text) = ##f
  }

  \context {
    \Staff
    \override InstrumentName #'font-size = #1.25
    % Default value is 1/4, will be changed only if necessary
    tupletSpannerDuration = #(ly:make-moment 1 4)
    \accidentalStyle "modern"
  }
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
    \override InstrumentName #'font-size = #1.25
    connectArpeggios = ##t %TODO: move to ulLibrary
    \accidentalStyle "piano"

  }
  \context {
    \Dynamics
    \consists "Bar_engraver"
    \consists "Separating_line_group_engraver"
    \override BarLine #'transparent = ##t
  }
}
