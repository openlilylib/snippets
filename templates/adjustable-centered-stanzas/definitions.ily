\version "2.19.3"

\header {
  snippet-title = "Adjustable centered stanzas"
  snippet-author = "Janek Warchoł and Thomas Morley"
  snippet-description = \markup {
    When typesetting songs, you usually want to put additional stanzas
    below the music.  This function will take care of all the
    boilerplate markup code that you need for that.
  }
  % add comma-separated tags to make searching more effective:
  tags = "stanza, stanzas, song, vocal music, centered, markup, fill-line"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
Possible improvements:
- punctuation after stanza number could be parametrized
- a property for specifying number placement could be added (top or left)
- first parameter could be changed into a list that would
  specify exactly how many stanzas should be in each column
  (like this: '(2 3 2) ).  This could be useful if the stanzas
  have uneven heights and the user wants to balance total height
  of the columns.

However, i don't think we have to make these changes until we
have a proof that they are really needed.
%}

#(define (split-lst lst n)
   "Returns @var{lst} split into equal-sized sublists:
(split-lst '(1 2 3 4) 2) -> '((1 2)(3 4))
Roundings may happen:
(split-lst '(1 2 3 4 5) 2) -> '((1 2 3)(4 5))"
   (define (helper accum remain k)
     (if (= k 0)
         (reverse accum)
         (let* ((elems-left (length remain))
                (next-sublist-size (ceiling (/ elems-left k))))
           (helper (cons (list-head remain next-sublist-size) accum)
             (list-tail remain next-sublist-size)
             (- k 1)))))
   (helper '() lst n))

#(define (insert-separator lst separator)
   "Insert @var{separator} between elements of @var{lst}."
   (define (helper accum remain)
     (if (null? (cdr remain))
         (reverse (cons (car remain) accum))
         (helper (append (list separator (car remain)) accum) (cdr remain))))
   (helper '() lst))

#(define (enclose-list lst elem)
   "Add @var{elem} at the beginning and end of @var{lst}."
   (cons elem (append lst (list elem))))

#(define-markup-command (stanzas-in-columns layout props column-count stanza-list)
   (number? markup-list?)
   #:properties ((vertical-spacing 1.5)
                 (horizontal-spacing 1.5)
                 (first-number 2)
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
          (format-stanza-numbers
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
              (map format-stanza-numbers m))
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
           % note that \fill-line would give worse spacing.
           \justify-line
           % By default match the font size of the lyrics. We don't use \large
           % because of https://code.google.com/p/lilypond/issues/detail?id=3947
           \scale #`(1.12 . 1.12)
           % Adjust the distace between text lines (for cramped spacing).
           \override #`(baseline-skip . ,line-spacing)
           % Compress text horizontally (for cramped spacing).
           \scale #`(,horizontal-compression . 1)
           % \box % <- useful for debugging
           \scale #extra-scaling #ready-to-use
         }
       #})))
