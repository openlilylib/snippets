\version "2.17.3"

% TODO: see what's special here. add oll/snippets headers, add example.
% probably change some names, don't use proprietary font, remove spurious comments(?)
% update syntax to 2.18?

\header {
  % Dummy header fields
  % Used to see whether the actual values are still missing
  poet = "Dummy poet"
  % opusmain and opussub should be entered separately
  % and will be concatenated (with thin spaces) in bookTitleMarkup
  opusmain = "X"
  opussub = "x"

  % copyright and tagline are replaced by oddFooterMarkup 
  copyright = ""
  tagline = ""

}

\paper {
  #(include-special-characters)

  bookTitleMarkup = \markup {
    \column {
      \fontsize #1 \fill-line {
        \center-column {
          \override #'(font-name . "Minion Pro Italic")
          \raise #1.5 \fromproperty #'header:dedication

          \override #'(font-name . "Minion Pro Bold")
          \fontsize #5.2 { \fromproperty #'header:title }
          \override #'(font-name . "Minion Pro")
          \lower #0.45 \concat { "(" \fromproperty #'header:poet ")" }
        }
      }
      \fill-line {
        \override #'(font-name . "Minion Pro Italic")
        \fontsize #1 \lower #1 \fromproperty #'header:originalkey
        \override #'(font-name . "Minion Pro Medium")
        \fontsize #1.25 \lower #1 \concat {
          "Oskar Fried, "
          "op.&thinsp;"
          \fromproperty #'header:opusmain
          ", Nr.&thinsp;"
          \fromproperty #'header:opussub
        }
      }
    }
  }


  oddHeaderMarkup = \markup
  \fill-line {
    % insert the current date and the 'progress' string from the header
    % % TODO: check if this can be automated with draftMode and pubMode
    \on-the-fly #first-page \progressString
    \on-the-fly #print-page-number-check-first
    \override #'(font-name . "Minion Pro Semibold")
    \fontsize #1.15
    % Page numbers are displayed in draftMode
    % and hidden in pubMode
    % \pageNumber is defined in includes/library/showPageNumbers.ily,
    % defaults to invisible (global.ily) 
    % and changed to visible in draftMode.ily
    \pageNumber
  }

  %% evenHeaderMarkup would inherit the value of
  %% oddHeaderMarkup if it were not defined here
  evenHeaderMarkup = \markup
  \fill-line {
    \override #'(font-name . "Minion Pro Medium")
    \fontsize #1.15
    \on-the-fly #print-page-number-check-first
    % See explanation in oddHeaderMarkup
    \pageNumber
    % insert the current date and the 'progress' string from the header
    % % TODO: check if this can be automated with draftMode and pubMode
    \on-the-fly #first-page \progressString

  }

  oddFooterMarkup = \markup {
    \on-the-fly #first-page {
      \fill-line {
        {
          \concat
          {
            \lower #0.25 "&copyright; "
            \override #'(font-name . "Present Lt Std Bold") \fontsize #1.5 "sound-rel "
          }
        }
        {
          % This string is printed in white instead of being left out
          % In the final book it will be printed by LaTeX.
          % Leaving it out would change the vertical spacing of the score
          % (Layout was defined when the string was still printed)
          % so we decided to just white it out.
          \with-color #white {
            \caps "dre 1201"
          }
        }
        \null
      }
    }
    \on-the-fly #not-first-page {
      \fill-line {
        \null
        % See comment above
        \with-color #white
        {
          \caps "dre 1201"
        }
        \null
      }
    }
  }
  evenFooterMarkup = \oddFooterMarkup
}