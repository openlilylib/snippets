\version "2.19.3"

\header {
  snippet-title = "Adjustable centered stanzas"
  snippet-author = "Janek Warchoł"
  snippet-description = \markup {
    When typesetting songs, you usually want to put additional stanzas
    below the music.  These two functions will take care of all the
    boilerplate markup code that you need for that.
  }
  % add comma-separated tags to make searching more effective:
  tags = "stanza, stanzas, song, vocal music, centered, markup, fill-line"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Two definitions for splitting a list in equal sized sublists.
%% They differ how the case is handled, if a remainaining rest can't be divided
%%
%% In this example the list should be divided into three parts:
%% The first may do:  (split-lst '(1 2 3 4) 3) -> ((1 2) (3) (4))
%% The second:        (split-lst '(1 2 3 4) 3) -> ((1 2) (3 4))
%% Not sure which definition is apropiate. Personally I'd prefer the second for
%% use with the stanza-markup-command.

%{
#(define (split-lst lst n)
"
 Returns a new list containing equal sized new lists
 (split-lst '(1 2 3 4) 2) -> '((1 2)(3 4))
 Roundings may happen:
 (split-lst '(1 2 3 4 5) 2) -> '((1 2 3)(4 5))
"
  (define (helper l1 l2 k)
   (if (= k 0)
       (reverse l2)
       (let* ((len (length l1))
              (val (ceiling (/ len k))))
         (helper (list-tail l1 val) (cons (list-head l1 val) l2) (- k 1)))))
  (helper lst '() n))
%}

#(define (split-lst lst n)
   "
 Returns a new list containing equal sized new lists
 (split-lst '(1 2 3 4) 2) -> '((1 2)(3 4))
 Roundings may happen:
 (split-lst '(1 2 3 4 5 6 7) 4) -> '((1 2) (3 4) (5 6) (7))
"
   (let* ((len (length lst))
          (val (ceiling (/ len n))))

     (define (helper l1 l2 v k)
       (if (or (> v (length l1)) (= k 0))
           (if (null? l1)
               (reverse l2)
               (append (reverse l2) (list l1)))
           (helper (list-tail l1 v) (cons (list-head l1 v) l2) v (- k 1))))
     (helper lst '() val n)))

#(define (insert-separator lst separator)
   "Inserts @var{separator} between elements of @var{lst},
returning this new list."
   (define (helper accum remain)
     (if (null? (cdr remain))
         (reverse (cons (car remain) accum))
         (helper (append (list separator (car remain)) accum) (cdr remain))))
   (helper '() lst))

#(define (enclose-list lst elem)
   "Add @var{elem} to the beginning and end of @var{lst}."
   (cons elem (append lst (list elem))))

#(define-markup-command (stanzas layout props stanza-list)
   (markup-list?)
   #:properties ((vertical-spacing 1.5)
                 (horizontal-spacing 1.5)
                 (first-number 2)
                 (column-count 1)
                 (extra-scaling '(1 . 1)))
   (let* ((null-mrkp #{ \markup \null #})
          (stanza-vdist #{ \markup \vspace #vertical-spacing #})
          (number-hdist #{ \markup \hspace #horizontal-spacing #})
          (lily-default-baseline-skip 3)
          (line-spacing (* lily-default-baseline-skip
                          (min 1 (+ 0.6 (* 0.4 vertical-spacing)))))
          (horizontal-compression (min 1 (+ 0.75 (* 0.25 horizontal-spacing))))
          (counted-stanza-list
           (map
            (lambda (e)
              (cons (+ first-number (cdr e) -1) (car e)))
            (count-list stanza-list)))
          (mrkps-for-columns (split-lst counted-stanza-list column-count))
          (make-stanza-text-markup
           (lambda (mrkp)
             #{
               \markup \line {
                 \bold #(string-append (number->string (car mrkp)) ".")
                 #number-hdist
                 #(cdr mrkp)
               }
             #}))
          (raw-mrkp-lists
           (map
            (lambda (m)
              (map make-stanza-text-markup m))
            mrkps-for-columns))
          ;; insert vertical-space between elements of sublists
          (mrkp-list-with-vertical-space
           (map
            (lambda (m)
              (insert-separator m stanza-vdist))
            raw-mrkp-lists))
          ;; makes column-markups
          (ready-columned-mrkp-list
           (map
            (lambda (m)
              (make-column-markup m))
            mrkp-list-with-vertical-space))
          ;; insert null-markup at start, end, and between columns
          (ready-to-use
           (enclose-list
            (insert-separator ready-columned-mrkp-list null-mrkp)
            null-mrkp)))
     (interpret-markup layout props
       #{
         \markup {
           \justify-line
           % By default match the font size of the lyrics. We don't use \large
           % because of https://code.google.com/p/lilypond/issues/detail?id=3947
           \scale #`(1.12 . 1.12)
           % Adjust the distace between text lines (for cramped spacing).
           \override #`(baseline-skip . ,line-spacing)
           % Compress text horizontally (for cramped spacing).
           \scale #`(,horizontal-compression . 1)
           % \box % useful for debugging
           \scale #extra-scaling #ready-to-use
         }
       #})))
