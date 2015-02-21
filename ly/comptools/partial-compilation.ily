\header {
  oll-title = "Clip regions to be compiled independently"
  oll-author = "Urs Liska"
  oll-short-description = \markup {
    Partially compile a score, setting a clipping range.
  }
  oll-description = \markup \justify {
    Using the edition-engraver (which is also part of openLilyLib)
    this module allows to specify a region of the score (in terms of
    bar (and optionally page) numbers that is compiled partially.
    This can speed up work on larger scores when you are only interested
    in a certain region of the score.
  }
  oll-usage = \markup \justify {
    Use \typewriter { "\\setClipRegion from to" } as a toplevel command
    somewhere after including this file. \typewriter from and \typewriter to
    are barnumbers or lists with a barnumber and a fraction
    (e.g. \typewriter { "\\setClipRegion #'(12 (45 2/4))" } for a range from
    measure 12 to the \italic third crotchet in measure 45.
    If they are out of range they are simply ignored so the
    score will be engraved from the beginning and/or through the end.
    You can also use the module to compile a given page or page range
    (e.g. from an original score) if you provide a list beforehand.
    This list has to be named \typewriter originalPageBreaks
    and simply contains a list of barnumbers. The available commands are
    \typewriter { setClipPage num } and \typewriter { setClipPageRange from to }.
    They perform a number of plausibility/validity checks.
  }
  oll-category = "compilation-tools"
  % add comma-separated tags to make searching more effective:
  oll-tags = "compilation,workflows"
  % is this snippet ready?  See meta/status-values.md
  oll-status = "ready"

  oll-todo = \markup {
    Add a parallel structure with arbitrary compilation points
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define variables holding the conditional breaks.
% They expect lists with breaks. Each break can be
% - an integer representing the bar number
% - a list with a barnumber and a fraction
\registerOption comptoools.line-breaks #'()
\registerOption comptools.page-breaks #'()
\registerOption comptools.page-turns #'()

% This functionality relies on the edition-engraver
% which is also part of openLilyLib
\include "editorial-tools/edition-engraver/definitions.ily"
\addEdition clips

% Initialize general clipping variables
#(define do-clip-region #f)
#(define clip-region-from 1)
#(define clip-region-to 1)

% Define (and activate) a clipping range.
% Only this range is typeset and compiled.
% Expect warnings about incomplete ties, dynamics etc. or other warnings/errors.
% If one of the arguments is out of range it is simply ignored
% (if #to is greater than the number of measures in the score
%  the score is engraved to the end).
setClipRegion =
#(define-void-function (parser location from to)
   (memom? memom?)
   (let ((clip-region-from
          (if (integer? from)
              (list from #{ 0/4 #})
              (list (car from)
                (ly:make-moment (numerator (cadr from))(denominator (cadr from))))))
         (clip-region-to
          (if (integer? to)
              (list (+ 1 to) #{ 0/4 #})
              (list (car to)
                (ly:make-moment (numerator (cadr to))(denominator (cadr to)))))))
   #{
     \editionMod clips 1 0/4 clip-regions.Score.A
     \set Score.skipTypesetting = ##t
     \editionMod clips #(car clip-region-from) #(cadr clip-region-from) clip-regions.Score.A
     \set Score.skipTypesetting = ##f
     \editionMod clips #(car clip-region-to) #(cadr clip-region-to) clip-regions.Score.A
     \set Score.skipTypesetting = ##t
   #}))

% define (and activate) a page range to be compiled alone.
% Pass first and last page as integers.
% Several validity checks are performed.
setClipPageRange =
#(define-void-function (parser location from to)
   (integer? integer?)
   (let* ((page-breaks #{ \getOption comptools.page-breaks #})
         (page-count (length page-breaks)))
   (if (= 0 page-count)
       (oll:warn "\\setClipPageRange requested, but no original page breaks defined. 
Continuing by compiling the whole score.~a""")
       ;; We do have page breaks so continue by retrieving barnumbers from that list
       (cond
        ((> from to)
         (oll:warn "\\setClipPageRange: Negative page range requested. 
Continuing by compiling the whole score.~a" ""))
        ((< from 1)
         (oll:warn "\\setClipPageRange: Page number below 1 requested. 
Continuing by compiling the whole score.~a" ""))
        ((> to (+ 1 page-count))
         (oll:warn "\\setClipPageRange: Page index out of range (~a). 
Continuing by compiling the whole score."
         (format "from ~a to ~a requested, ~a available" from to page-count)))
        (else
         (let ((from-bar (if (eq? from 1)
                             ;; First page is not included in the originalPageBreaks list
                             ;; so we set the barnumber to 1
                             1
                             (list-ref page-breaks (- from 2))))
               (to-bar (if (eq? to (+ (length page-breaks) 1))
                           ;; There is no page break *after* the last page,
                           ;; so we just set the "to" barnumber to -1
                           ;; because this simply discards the argument and compiles through to the end
                           -1
                           ;; Otherwise we look up the barnumber for the page break and subtract 1
                           ;; (the last measure to be included is the last one from the previous page
                           (- (list-ref page-breaks (- to 1)) 1))))
           #{ \setClipRegion #from-bar #to-bar #}))))))

% Define (and activate) a page to be compiled alone.
% Only that page is typeset
setClipPage =
#(define-void-function (parser location page)
   (integer?)
   #{ \setClipPageRange #page #page #})

\layout {
  \context {
    \Score
    \consists \editionEngraver clip-regions
  }
}
