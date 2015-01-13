\header {
  oll-title = "Insert conditional breaks"
  oll-author = "Urs Liska"
  oll-short-description = \markup {
    Take lists of conditional breaks and apply them
  }
  oll-description = \markup \justify {
  }
  oll-usage = \markup \justify {
  }
  oll-category = "compilation-tools"
  % add comma-separated tags to make searching more effective:
  oll-tags = "compilation,layout-control"
  % is this snippet ready?  See meta/status-values.md
  oll-status = "ready"

  oll-todo = \markup {
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% This functionality relies on the edition-engraver
% which is also part of openLilyLib
\include "editorial-tools/edition-engraver/definitions.ily"
\addEdition conditional-breaks

% Make sure the configuration variables are all present
% and initialize them to #f if not
#(cond ((not (defined? 'keep-conditional-breaks))
        (define keep-conditional-breaks #f)))
#(cond ((not (defined? 'keep-conditional-page-breaks))
        (define keep-conditional-page-breaks #f)))
#(cond ((not (defined? 'keep-conditional-page-turns))
        (define keep-conditional-page-turns #f)))

% Initialize the conditional break lists to empty lists
% if they don't exist yet.
% These are lists with barnumbers for each conditional break
% Instead of an integer a break can also be a list of
% barnumber and position (with positions being zero-based),
% e.g (12 1/4) for the *second* quarter in measure 12.
#(cond ((not (defined? 'conditionalLineBreaks))
        (define conditionalLineBreaks '())))
#(cond ((not (defined? 'conditionalPageBreaks))
        (define conditionalPageBreaks '())))
#(cond ((not (defined? 'conditionalPageTurns))
        (define conditionalPageTurns '())))

% Conditionally apply the breaks defined in the lists.
% (Non-present lists are taken as empty lists)
#(if keep-conditional-breaks
     (let* (
             ;; if we do respect original page breaks linebreaks are simply linebreaks,
             ;; but if we do not respect page breaks, page breaks and turns are
             ;; realized as line breaks instead.
             (linebreaks (if keep-conditional-page-breaks
                             conditionalLineBreaks
                             (append conditionalLineBreaks conditionalPageBreaks conditionalPageTurns)))
             ;; if we do not respect page breaks we use an empty list
             (pagebreaks (if keep-conditional-page-breaks
                             conditionalPageBreaks
                             '()))
             ;; if we do not respect page turns we use an empty list
             (pageturns (if keep-conditional-page-turns
                            conditionalPageTurns
                            '())))

       #{
         \editionModList conditional-breaks conditional-breaks.Score.A
         \break #linebreaks
         \editionModList conditional-breaks conditional-breaks.Score.A
         \pageBreak #pagebreaks
         \editionModList conditional-breaks conditional-breaks.Score.A
         \pageTurn #pageturns
       #}))

\layout {
  \context {
    \Score
    \consists \editionEngraver conditional-breaks
  }
}
