\version "2.19.15"

%{
  So far this implements logging breaks of a compilation.
  If a log file is present the results from that logging can be read back
  and used to recompile a portion representing the current (an arbitrary) system.

  What has to be done (maybe in another file?) is:
  - set up a system-wise compilation
  - recompile using a given system plus one measure before and after
  - set paper variables for recompilation
    - no titles
    - no indent
    - maybe ragged-right?
  - try to determine open spanners and add corresponding items in the before/after bars
%}

#(use-modules (ice-9 rdelim))

\include "general-tools/clip-regions/definitions.ily"

\header {
  title = "Test"
}


%\include "compile-by-systems.ily"

% Define log file name for break information
#(define breaks-output-file
   (format "~a.breaks.log" (ly:parser-output-name parser)))

% Read existing log file
% TODO: This currently does *not* have any checks.
#(ly:parser-include-string
  parser
  ;; We have to append two parens here because they can't be written
  ;; during the output stage (TODO: Fix that, find the way to process the file after compilation)
  (format "~a ))"
    (read-line (open-file breaks-output-file "r"))))

% Reading the file should produce a list with break informations
% from the previous compilation, formatted as a editionModList argument
#(display current-breaks)

#(define conditionalPageBreaks current-breaks)
%\setClipPage 3


% Initialize output file
#(define out (open-output-file breaks-output-file))
#(format out "#(define current-breaks '(")

% Determine current break position and append it to log file
#(define (display-breaks grob)
   (if (and (grob::has-interface grob 'paper-column-interface)
            (eq? #t (ly:grob-property grob 'non-musical)))
       (if (eq? 1 (ly:item-break-dir grob))
           (let* ((location (ly:grob-property grob 'rhythmic-location))
                  (m (format "~a" (car location)))
                  (frac (moment->fraction (cdr location)))
                  (brk
                   (if (= 0 (car frac))
                       (format " ~a" m)
                       (format " (~a ~a)" m (/ (car frac) (cdr frac))))))
             (format out "~a" brk)))
       (ly:message "Need NonMusicalPaperColumn grob to determine line breaks.")))

\layout {
  \context {
    \Score
    \override NonMusicalPaperColumn.after-line-breaking = #display-breaks
  }
}


{
  \repeat unfold 6 { c'' d'' e'' f'' }
  c'' d'' \bar "" \break e'' f''
  \repeat unfold 100 { c'' d'' e'' f'' }
}

