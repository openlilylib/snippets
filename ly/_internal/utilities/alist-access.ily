%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib/openlilylib                                 %
%              -----------                                                    %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2015                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.17.96"

\header {
  snippet-title = "work with (nested) association-lists"
  snippet-author = "Jan-Peter Voigt, Urs Liska"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "Program flow, LilyPond variables, a-lists"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Convert a list to a dot-path notation string.
% Can be used to print/log tree paths
#(define (symbol-list->dot-path path)
   "output option path list as a dot-delimited string"
   (string-join
    (map
     (lambda (p)
       (symbol->string p))
     path) "."))

% after \clralist <name> the variable <name> is initialized with an empty list
#(define-public clralist
   (define-void-function (parser location alst)(symbol?)
     (if (lilypond-greater-than? "2.19.21")
         (ly:parser-define! alst (list))
         (ly:parser-define! parser alst (list)))
     ))
% sets one value - replaces the value and leaves the order of elements, if <name> is already present
#(define-public setalist
  (define-void-function (parser location alst name val)(symbol? symbol? scheme?)
    (let ((l (if (lilypond-greater-than? "2.19.21")
                 (ly:parser-lookup alst)
                 (ly:parser-lookup parser alst)))
          (setv #t))
      (set! l (map (lambda (p)
                     (if (and (pair? p) (equal? (car p) name))
                         (begin
                          (set! setv #f)
                          (cons name val))
                         p
                         )) l))
      (if setv (set! l (append l (list (cons name val)))))
      (if (lilypond-greater-than? "2.19.21")
          (ly:parser-define! alst l)
          (ly:parser-define! parser alst l)))))
% sets one value - <name> is always placed at the end of the list
#(define-public addalist
  (define-void-function (parser location alst name val)
    (symbol? symbol? scheme?)
    (let ((l (if (lilypond-greater-than? "2.19.21")
                 (ly:parser-lookup alst)
                 (ly:parser-lookup parser alst))))
      (set! l (filter (lambda (p) (and (pair? p)(not (equal? (car p) name)))) l))
      (if (lilypond-greater-than? "2.19.21")
          (ly:parser-define! alst (append l (list (cons name val))))
          (ly:parser-define! parser alst (append l (list (cons name val)))))
      )))
% removes one entry from association list
#(define-public remalist
  (define-void-function (parser location alst name)(symbol? symbol?)
    (let ((l (if (lilypond-greater-than? "2.19.21")
                 (ly:parser-lookup alst)
                 (ly:parser-lookup parser alst))))
      (if (lilypond-greater-than? "2.19.21")
          (ly:parser-define! alst
            (filter (lambda (p) (and (pair? p)(not (equal? (car p) name)))) l))
          (ly:parser-define! parser alst
            (filter (lambda (p) (and (pair? p)(not (equal? (car p) name)))) l)))
      )))

% get entry from nested a-list
#(define-public (get-a-tree parser location name path)
   (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
   (let ((opts (if (lilypond-greater-than? "2.19.21")
                   (ly:parser-lookup name)
                   (ly:parser-lookup parser name))))
     (define (getval ol op)
       (let ((sym (car op)))
         (cond
          ((> (length op) 1)
           (let ((al (assoc-get sym ol #f)))
             (if (list? al)
                 (getval al (cdr op))
                 #f)))
          ((= (length op) 1)
           (let ((valpair (assoc (car op) ol)))
             (if valpair
                 (cdr valpair)
                 ; return unspecified if key is not present
                 ))))))
     (if (list? opts)
         (getval opts path)
         (begin
          (oll:warn location "~A is not list (~A)" name opts)
          #f)
         )))
% add an entry to a nested a-list
#(define (add-a-tree parser location name sympath val assoc-set-append)
   (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
   (let ((opts
          (if (lilypond-greater-than? "2.19.21")
              (ly:parser-lookup name)
              (ly:parser-lookup parser name)
              )))
    (define (setval ol op)
      (let ((sym (car op))
            (ol (if (list? ol) ol (begin
                                   (oll:warn location "deleting '~A'" ol)
                                   '()))))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (if (not (list? al))
                  (begin
                   (oll:warn location "deleting '~A' = '~A'" sym al)
                   (set! al '())
                   ))
              (assoc-set-append ol sym (setval al (cdr op)))
              )
            (let ((ov (assoc-get sym ol #f)))
              ;(if ov (oll:warn location "deleting '~A'" ov))
              (assoc-set-append ol sym val)
              )
            )))
    (set! opts (setval opts sympath))
     (if (lilypond-greater-than? "2.19.21")
         (ly:parser-define! name opts)
         (ly:parser-define! parser name opts))
     ))
% remove an entry from a nested a-list
#(define (rem-a-tree parser location name sympath)
  (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
  (let ((opts (if (lilypond-greater-than? "2.19.21")
                  (ly:parser-lookup name)
                  (ly:parser-lookup parser name))))
    (define (remval ol op)
      (let ((sym (car op)))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (set! al (remval al (cdr op)))
              (if (> (length al) 0)
                  (map (lambda (p) (if (and (pair? p)(equal? (car p) sym)) (cons sym al) p)) ol)
                  (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol))
              )
            (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol)
            )
        ))
    (set! opts (remval opts sympath))
    (if (lilypond-greater-than? "2.19.21")
        (ly:parser-define! name opts)
        (ly:parser-define! parser name opts))
    ))

% clear/create an empty a-list
#(define-public clratree clralist)
% get entry from nested a-list
#(define-public getatree
   (define-scheme-function (parser location name sympath)(symbol? list?)
     (get-a-tree parser location name sympath)))
% add an entry to nested a-list at the end
#(define-public addatree
  (define-void-function (parser location name sympath val)(symbol? list? scheme?)
    (add-a-tree parser location name sympath val
      (lambda (l sym val)
        (append (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) l)
          (list (cons sym val)))))))
% set an entry in nested a-list in place
#(define-public setatree
  (define-void-function (parser location name sympath val)(symbol? list? scheme?)
    (add-a-tree parser location name sympath val
      (lambda (l sym val) (assoc-set! l sym val)))))
% remove an entry from nested a-list
#(define-public rematree
  (define-void-function (parser location name sympath)(symbol? list?)
    (rem-a-tree parser location name sympath)))
