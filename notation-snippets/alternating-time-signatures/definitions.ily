\version "2.18.2"

\header {
  snippet-title = "Print (irregularly) changing meters"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    This snippet allows you to print a list of time signatures
    indicating (irregularly) changing meters.
  }
  % add comma-separated tags to make searching more effective:
  tags = "polymetrics, time signature"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished, buggy"


  %{
    TODO:
    - Complete documentation
    - incorporate \fractionList to LilyPond
      and once that's finished use a version switch that only
      defines the function with earlier LilyPond versions.#

    - Check the sublists for validity:
      - one number: issue a warning
      - one string: create a column from that (-> use case: hyphen)
        (saw comment https://github.com/openlilylib/snippets/commit/b1cceaadd43dda8f162e2fa0591bce68fd1af277#commitcomment-6286137
         so it seems better to create that for an empty sublist?)
  %}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the core function that should go into LilyPond
fractionList =
#(define-scheme-function (parser location timesigs) (list?)
   (_i "Generate a list of time signature markups that can be used
to override TimeSignature.stencil in order to indicate irregularly
changing meters.")
   (lambda (grob)
     (grob-interpret-markup grob
       #{ \markup \override #'(baseline-skip . 0)
          \number
          #(map (lambda (x) #{ \markup \center-column #(map number->string x) #})
             timesigs)
       #})))

% It's recommended in Behind Bars to use hyphen
% between time signatures for irregular alternation
gould-irreg =
#(define-scheme-function (parser location timesigs) (list?)
   (_i "Generate a list of time signature markups that can be used
to override TimeSignature.stencil in order to indicate irregularly
changing meters.")
   (let ((lastsig (car (reverse timesigs))))
   (lambda (grob)
     (grob-interpret-markup grob
       #{ \markup \override #'(baseline-skip . 0)
          \number
          #(map (lambda (x)
                  #{ \markup {
                    \center-column #(map number->string x)
                    #(if (eq? x lastsig)
                         "" (markup
                             #:line
                             (#:override
                              (cons (quote thickness) 3.4)
                              (#:draw-line (cons -0.9 0)))))
                  } #})
             timesigs)
       #}))))

% This is a function to make it more accessible in standard cases
alternatingTimeSignatures =
#(define-music-function (parser location timesigs) (list?)
   (let ((first-effective-timesig
          (cons
           (caar timesigs)
           (cadar timesigs))))
     #{
       \once \override Score.TimeSignature.stencil = \fractionList #timesigs
       \time #first-effective-timesig
     #}))
