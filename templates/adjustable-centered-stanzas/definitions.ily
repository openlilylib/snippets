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

#(define-markup-command (stanzas-in-one-column layout props stanza-list)
   (markup-list?)
   #:properties ((stanza-vdist 1)
                 (number-hdist 1)
                 (first-number 2)
                 (text-scaling '(1 . 1))
                 (line-spacing 1))
   (let* ((amount (length stanza-list))
          (numbers (map (lambda (x) (+ x first-number)) (iota amount))))
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
           \scale #text-scaling {
             \null

             \column {
               #(map
                 (lambda (num text)
                   #{
                     \markup \column {
                       \line {
                         \bold \concat { #(number->string num) "." }
                         \hspace #number-hdist
                         #text
                       }
                       \vspace #stanza-vdist
                     }
                   #})
                 numbers
                 stanza-list)
             }

             \null
           }
         }
       #})))

#(define-markup-command (stanzas-in-two-columns layout props first-col second-col)
   (markup-list? markup-list?)
   #:properties ((stanza-vdist 1)
                 (number-hdist 1)
                 (first-number 2)
                 (text-scaling '(1 . 1))
                 (line-spacing 1))
   (let* ((amount-first (length first-col))
          (amount-second (length second-col))
          (numbers-first (map (lambda (x) (+ x first-number)) (iota amount-first)))
          (numbers-second (map (lambda (x) (+ x 1 (last numbers-first))) (iota amount-second))))
     (interpret-markup layout props
       #{
         \markup {
           \justify-line
           \override #`(baseline-skip . ,(* 3 line-spacing))
           \scale #'(1.12 . 1.12) % by default match the size of the lyrics
           \scale #text-scaling {
             \null

             \column {
               #(map
                 (lambda (num text)
                   #{
                     \markup \column {
                       \line {
                         \bold \concat { #(number->string num) "." }
                         \hspace #number-hdist
                         #text
                       }
                       \vspace #stanza-vdist
                     }
                   #})
                 numbers-first
                 first-col)
             }

             \null

             \column {
               #(map
                 (lambda (num text)
                   #{
                     \markup \column {
                       \line {
                         \bold \concat { #(number->string num) "." }
                         \hspace #number-hdist
                         #text
                       }
                       \vspace #stanza-vdist
                     }
                   #})
                 numbers-second
                 second-col)
             }

             \null
           }
         }
       #})))
