% Formats for example pages in openlilylib documentation

#(define include-name
   #{ \getOption documentation.include-file #})

#(define include-clause
   (format "\\loadModule \"~a\"" include-name))

\loadModule #include-name

#(ly:set-option 'relative-includes #t)

% This is still optional. I thought of providing a certain binding margin.
\paper {
  left-margin = 2\cm
  right-margin = 1.5\cm
}

% Use this to consistently lay out the documentation
#(define-markup-command (section layout props heading) (markup?)
   (interpret-markup layout props
     #{
       \markup \column {
         \vspace #1
         \bold #heading
         \vspace #0.5
     }#}))





% Consistently format and process headers and footers,
% populated with data from the snippet's file
\include "titling.ily"
