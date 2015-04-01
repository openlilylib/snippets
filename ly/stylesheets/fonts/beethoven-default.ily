%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of Stylesheets,                                           %
%                      ===========                                            %
% a library to manage and use style sheets and alternative fonts with         %
% the GNU LilyPond engraving software,                                        %
% belonging to openLilyLib (https://github.com/openlilylib/openlilylib        %
%              -----------                                                    %
%                                                                             %
% Stylesheets is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% Stylesheets is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with Stylesheets.  If not, see <http://www.gnu.org/licenses/>.        %
%                                                                             %
% Stylesheets is maintained by                                                %
% - Urs Liska, ul@openlilylib.org                                             %
% - Kieren MacMillan, kieren_macmillan@sympatico.ca                           %
% - Abraham Lee, tisimst.lilypond@gmail.com                                   %
% Copyright Urs Liska / Kieren MacMillan, Abraham Lee, 2015                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Default stylesheet for the Beethoven font.
  Provided by Kieren MacMillan (unprocessed)
%}

#(define (expand-repetitions arg)
;; 4*5 --> 4 4 4 4 4
;; (at any level of nesting)
  (fold-right
    (lambda (elem prev)
            (cond ((pair? elem)
                   (cons (expand-repetitions elem) prev))
                  ((symbol? elem)
                   (let* ((str (symbol->string elem))
                          (split (string-split str #\*))
                          (split (map (lambda (elem) (string->number elem)) split)))
                     (append (make-list (cadr split) (car split))
                             prev)))
                  (else (cons elem prev))))
    '()
    arg))

#(define ((bars-per-line-systems-per-page-engraver lst) ctx)
  (let* ((bars-and-systems? (any pair? lst))
         (working-copy (expand-repetitions lst))
         (systems-per-page
           (if bars-and-systems?
               (car working-copy)
               #f))
         (last-measure-seen (ly:context-property ctx 'currentBarNumber))
         (last-measure-seen (if (null? last-measure-seen)
                                0
                                (1- last-measure-seen)))
         (total
           (if systems-per-page
               (+ (car systems-per-page) last-measure-seen 1)
               (+ (car working-copy) last-measure-seen 1))))
  `((stop-translation-timestep
      . ,(lambda (trans)
          (let ((internal-bar (ly:context-property ctx 'internalBarNumber))
                (current-col (ly:context-property ctx 'currentCommandColumn)))
            ;; we are only interested in the first NonMusicalPaperColumn of
            ;; each measure
            (if (and (> internal-bar last-measure-seen)
                     (= (remainder internal-bar total) 0)
                     (pair? working-copy))
                (begin
                  (set! (ly:grob-property current-col 'line-break-permission) 'force)
                  (set! last-measure-seen internal-bar)
                  (if bars-and-systems?
                      (begin
                        (if (null? (cdr systems-per-page))
                            (begin
                              (set! (ly:grob-property current-col 'page-break-permission) 'force)
                              (if (pair? (cdr working-copy))
                                  (begin
                                    (set! working-copy (cdr working-copy))
                                    (set! systems-per-page (car working-copy)))
                                  (set! working-copy '())))
                            (set! systems-per-page (cdr systems-per-page)))
                        (set! total (+ total (car systems-per-page))))
                      (begin
                        (if (null? (cdr working-copy))
                            (set! working-copy lst)
                            (begin
                              (set! working-copy (cdr working-copy))))
                              (set! total (+ total (car working-copy)))))))))))))

#(define-markup-command (when-property layout props symbol markp) (symbol? markup?)
  (if (chain-assoc-get symbol props)
      (interpret-markup layout props markp)
      empty-stencil))

#(define-markup-command (oval layout props arg)
 (markup?)
 #:properties ((thickness 1)
               (font-size 0)
               (oval-padding 0.75))
 (let ((th (* (ly:output-def-lookup layout 'line-thickness)
              thickness))
       (pad (* (magstep font-size) oval-padding))
       (m (interpret-markup layout props (markup #:hcenter-in 1.5 arg))))
   (oval-stencil m th pad (* pad 1.5))))

#(define (format-oval-barnumbers barnum measure-pos alt-number context)
 (make-oval-markup
  (robust-bar-number-function barnum measure-pos alt-number context)))

#(define ((custom-script-tweaks ls) grob)
  (let* ((type (ly:prob-property (assoc-ref (ly:grob-properties grob)
'cause) 'articulation-type))
         (tweaks (assoc-ref ls type)))
    (if tweaks
        (for-each (lambda (x) (ly:grob-set-property! grob (car x)
(cdr x))) tweaks)
        '())))

#(define Henle-scripts '(
   ("staccato" . ((padding . 0.4)))
   ("fermata" . ((padding . 0.75)))
   ))


\paper {
  markup-system-spacing #'padding = #2
  markup-system-spacing #'minimum-distance = #18
  system-system-spacing = #'((padding . 2.75) (minimum-distance . 8))
}

\layout {
  \context {
    \Score
    \override MetronomeMark.font-size = #1
    \override MetronomeMark.padding = #1.25
    \override Script.before-line-breaking = #(custom-script-tweaks Henle-scripts)
    \override Script.outside-staff-priority = ##f
    \override Slur.thickness = #2
    \override Slur.height-limit = #3
    \override Slur.ratio = #0.2
    \override BarLine.space-alist.next-note = #'(semi-fixed-space . 1.75)
    barNumberFormatter = #format-oval-barnumbers
    \override BarNumber.font-size = #-5
    \override BarNumber.self-alignment-X = #LEFT
    \override BarNumber.Y-offset = #5
  }
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
    \override InstrumentName.font-size = #5
    \override InstrumentName.padding = #0.5
    \override StaffGrouper.staff-staff-spacing.minimum-distance = #9
  }
  \context {
    \Dynamics
    \override DynamicTextSpanner.whiteout = ##t
    \override DynamicTextSpanner.layer = #2
    \override DynamicTextSpanner.font-size = #0
    \override DynamicTextSpanner.style = #'none
  }
}
