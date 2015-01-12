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
    Enter \typewriter { setClipRegion from to } as a toplevel command
    somewhere after including this file. \typewriter from and \typewriter to
    are barnumbers. If they are out of range they are simply ignored so the
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
   (integer? integer?)
   (set! do-clip-region #t)
   (set! clip-region-from from)
   (set! clip-region-to (+ to 1))
   #{
     \editionMod clips 1 0/4 clip-regions.Score.A
     \set Score.skipTypesetting = ##t
     \editionMod clips #clip-region-from 0/4 clip-regions.Score.A
     \set Score.skipTypesetting = ##f
     \editionMod clips #clip-region-to 0/4 clip-regions.Score.A
     \set Score.skipTypesetting = ##t
   #})

% define (and activate) a page range to be compiled alone.
% Pass first and last page as integers.
% Several validity checks are performed.
setClipPageRange =
#(define-void-function (parser location from to)
   (integer? integer?)
   (if (not (defined? 'originalPageBreaks))
       (ly:warning "\\setClipPageRange requested, but no original page breaks defined. Continuing by compiling the whole score.")
       ;; We do have page breaks so continue by retrieving barnumbers from that list
       (cond
        ((> from to)
         (ly:warning "\\setClipPageRange: Negative page range requested. Continuing by compiling the whole score."))
        ((< from 1)
         (ly:warning "\\setClipPageRange: Page number below 1 requested. Continuing by compiling the whole score."))
        ((> to (+ 1 (length originalPageBreaks)))
         (ly:warning "\\setClipPageRange: Page index out of range. Continuing by compiling the whole score."))
        (else
         (let ((from-bar (if (eq? from 1)
                             ;; First page is not included in the originalPageBreaks list
                             ;; so we set the barnumber to 1
                             1
                             (list-ref originalPageBreaks (- from 2))))
               (to-bar (if (eq? to (+ (length originalPageBreaks) 1))
                           ;; There is no page break *after* the last page,
                           ;; so we just set the "to" barnumber to -1
                           ;; because this simply discards the argument and compiles through to the end
                           -1
                           ;; Otherwise we look up the barnumber for the page break and subtract 1
                           ;; (the last measure to be included is the last one from the previous page
                           (- (list-ref originalPageBreaks (- to 1)) 1))))
           #{ \setClipRegion #from-bar #to-bar #})))))

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
