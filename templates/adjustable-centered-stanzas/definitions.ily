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

%{
  TODO:
  There's a lot of ugly code duplication here - these functions should be merged
  into one generic function that would take the number of columns to use as an
  argument.
%}

%% Two definitions for splitting a list in equal sized sublists.
%% They differ how the case is handled, if a remaining rest can't be divided
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
   "
 Inserts @var{separator} between every element of @var{lst}, returning this
 new list.
"
   (define (helper l1 l2 to-insert)
     (if (null? (cdr l1))
         (reverse (cons (car l1) l2))
         (helper (cdr l1) (append (list to-insert (car l1)) l2) to-insert)))
   (helper lst '() separator))

#(define (add-to-begin-end-of-list lst add)
   "
 Returns a new list with @var{add} as first and last element of @var{lst}.
"
   (cons add (append lst (list add))))

#(define-markup-command (stanzas layout props stanza-list)
   (markup-list?)
   #:properties ((stanza-vdist 1)
                 (number-hdist 1)
                 (first-number 2)
                 (column-count 1)
                 (text-scaling '(1 . 1))
                 (line-spacing 1))
   (let* ((counted-stanza-list
           (map
            (lambda (e)
              (cons (+ first-number (cdr e) -1) (car e)))
            (count-list stanza-list)))
          (null-mrkp #{ \markup \null #})
          (vertical-space #{ \markup \vspace #stanza-vdist #})
          (mrkps-for-columns (split-lst counted-stanza-list column-count))
          (make-stanza-text-markup
           (lambda (mrkp)
             #{
               \markup %\column {
               %\line
               {
                 \bold #(string-append (number->string (car mrkp)) ".")
                 \hspace #number-hdist
                 #(cdr mrkp)
               }
               %  }
             #}))
          ;; Alternative procedure to get stanza-numbers above the text
          ;; TODO: appropiate calculation of line-width for this use-case
          ;(make-stanza-text-markup
          ;  (lambda (mrkp)
          ;    #{
          ;      \markup \center-column {
          ;          \override #'(line-width . 40)
          ;          \fill-line {
          ;            \italic #(string-append (number->string (car mrkp)) ".")
          ;          }
          ;          #(cdr mrkp)
          ;        }
          ;    #}))
          (raw-mrkp-lists
           (map
            (lambda (m)
              (map make-stanza-text-markup m))
            mrkps-for-columns))
          ;; insert vertical-space between elements of sublists
          (mrkp-list-with-vertical-space
           (map
            (lambda (m)
              (insert-separator m vertical-space))
            raw-mrkp-lists))
          ;; makes column-markups
          (ready-columned-mrkp-list
           (map
            (lambda (m)
              (make-column-markup m))
            mrkp-list-with-vertical-space))
          ;; insert null-markup at start/end and between every element
          (ready-to-use
           (add-to-begin-end-of-list
            (insert-separator ready-columned-mrkp-list null-mrkp)
            null-mrkp)))
     (interpret-markup layout props
       #{
         \markup {
           \justify-line
           % Adjust the distace between text lines (for cramped spacing).
           % 3 is the default value (it's independent from font size)
           \override #`(baseline-skip . ,(* 3 line-spacing))
           \scale #'(1.12 . 1.12) % by default match the size of the lyrics
           % We don't use \large, \small etc. because these commands
           % don't scale the distance between lines correctly.
           % First number - horizontal factor, 2nd - vertical.
           %\box % only inserted for better viewing
           \scale #text-scaling #ready-to-use
         }
       #})))
