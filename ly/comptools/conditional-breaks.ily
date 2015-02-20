\header {
  oll-title = "Insert conditional breaks"
  oll-author = "Urs Liska"
  oll-short-description = \markup {
    Take lists of conditional breaks and apply them
  }
  oll-description = \markup \justify {
    TODO
  }
  oll-usage = \markup \justify {
    TODO
  }
  oll-category = "page-layout"
  % add comma-separated tags to make searching more effective:
  oll-tags = "compilation,layout-control"
  % is this snippet ready?  See meta/status-values.md
  oll-status = "ready"

  oll-todo = \markup \wordwrap{
    Add an option to make the conditional breaks \italic exclusive.
    Currently they are simply applied, so LilyPond can also add its own
    breaks at will. However, it's not appropriate to hardcode this feature
    because that would \italic require the user to provide \italic complete
    lists.
  }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% This functionality relies on the edition-engraver
% which is also part of openLilyLib
\include "editorial-tools/edition-engraver/definitions.ily"
\addEdition conditional-breaks

% Configuration variables controlling the breaking behaviour
% By default line-breaks are respected
\registerOption comptools.conditional-breaks.use.line-breaks ##t
% If page-breaks are ##f and line-breaks ##t conditional page breaks
% are mapped to line breaks.
\registerOption comptools.conditional-breaks.use.page-breaks ##f
% Page turns are also mapped to line breaks if not explicitly activated.
\registerOption comptools.conditional-breaks.use.page-turns ##f

% Calling of this function is necessary to actually process the conditional breaks.
% Place it after all break lists have been set.
% - set: the named set as a Scheme symbol, e.g. \applyConditionalBreaks #'original-edition
applyConditionalBreaks =
#(define-void-function (parser location set)
   (symbol?)
   (let* (
           ;; configure which types of breaks are kept.
           ;; If page breaks or page turns are disabled they are not inserted.
           ;; However, if line breaks are enabled, page breaks and page turns
           ;; are inserted as line breaks.
           ;; Any combination should produce the expected results.
           (keep-conditional-line-breaks #{ \getOption comptools.conditional-breaks.use.line-breaks #})
           (keep-conditional-page-breaks #{ \getOption comptools.conditional-breaks.use.page-breaks #})
           (keep-conditional-page-turns #{ \getOption comptools.conditional-breaks.use.page-turns #})

           ;; Load a set of break positions.
           (break-set `(comptools break-sets ,set))
           (conditionalLineBreaks #{ \getChildOption #break-set #'line-breaks #})
           (conditionalPageBreaks #{ \getChildOption #break-set #'page-breaks #})
           (conditionalPageTurns #{ \getChildOption #break-set #'page-turns #})

           ;; process possible combinations of options
           (lbreaks (if keep-conditional-line-breaks
                        conditionalLineBreaks
                        '()))
           (lpbreaks (if keep-conditional-page-breaks
                         conditionalPageBreaks
                         '()))
           (lpturns (if keep-conditional-page-turns
                        conditionalPageTurns
                        '()))
           (linebreaks (if keep-conditional-line-breaks
                           (append lbreaks lpbreaks lpturns)
                           lbreaks))

           ;; if we do not respect page breaks we use an empty list
           (pagebreaks (if keep-conditional-page-breaks
                           lpbreaks
                           '()))
           ;; if we do not respect page turns we use an empty list
           (pageturns (if keep-conditional-page-turns
                          lpturns
                          '())))

     ;; apply the determined breaks as edition-engraver commands
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
