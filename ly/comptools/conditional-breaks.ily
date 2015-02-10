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
#(cond ((not (defined? 'keep-conditional-line-breaks))
        (define keep-conditional-line-breaks #f)))
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
#(let* (
         ;; configure which types of breaks are kept.
         ;; If page breaks or page turns are disabled they are not inserted.
         ;; However, if line breaks are enabled, page breaks and page turns
         ;; are inserted as line breaks.
         ;; Any combination should produce the expected results.
         (lbreaks (if keep-conditional-line-breaks
                      conditionalLineBreaks
                      '()))
         (lpbreaks (if (and keep-conditional-line-breaks
                            (not keep-conditional-page-breaks))
                       conditionalPageBreaks
                       '()))
         (lpturns (if (and keep-conditional-line-breaks
                           (not keep-conditional-page-turns))
                      conditionalPageTurns
                      '()))
         (linebreaks (if keep-conditional-page-breaks
                         conditionalLineBreaks
                         (append lbreaks lpbreaks lpturns)))
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
   #})

\layout {
  \context {
    \Score
    \consists \editionEngraver conditional-breaks
  }
}
