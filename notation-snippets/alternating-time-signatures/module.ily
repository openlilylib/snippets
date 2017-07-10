\version "2.18.2"

\include "../../general-tools/lilypond-version-predicates/definitions.ily"

\header {
  snippet-title = "Print (irregularly) changing meters"
  snippet-author = "Urs Liska, Peter Bjuhr"
  snippet-description = \markup {
    This snippet allows you to print a list of time signatures
    indicating (irregularly) changing meters.
    I defines a function that is built into LilyPond as of 2.19.7
    and adds a convenience function around it.

    Use 'fractionList' to override a TimeSignature's stencil
    or 'alternatingTimeSignatures' to do that and automatically
    set the first time signature to be effective.

    The functions expect a list of two-number lists as argument.
    Each sublist is interpreted as a time signature fraction.
    If a sublist does _not_ contain two numbers, a console
    warning will be printed, and the result of the function
    may be undefined.

    Elaine Gould suggests to print a hyphen between the
    time signatures, and this can be realized by passing a
    single "#t" as the first element of the argument list.
  }
  % add comma-separated tags to make searching more effective:
  tags = "polymetrics, time signature"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"

  %{
    TODO:
    - make the appearance of the hyphen configurable
  %}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the core function that should go into LilyPond
%
% For LilyPond versions before 2.19.7 the function fractionList
% is defined, for later versions the built-in function is used
#(if (not (defined? 'fractionList))
     (define fractionList
       (define-scheme-function (parser location timesigs) (list?)
         (let* (;; #t if the first list element is a #t,
                 ;; #f if it is anything else (i.e. a list)
                 (hyphen (and (boolean? (car timesigs))
                              (car timesigs)))
                 ;; = timesigs stripped off a possible leading boolean
                 (used-signatures
                  (if hyphen
                      (cdr timesigs)
                      timesigs)))
           ;; Check for well-formedness of the used-signatures list:
           ;; #t if all list elements are lists with two elements
           (if (every (lambda (sig) (eqv? 2 (length+ sig))) used-signatures)
               ;; generate the list of time signatures
               (lambda (grob)
                 (grob-interpret-markup grob
                   #{
                     \markup \override #'(baseline-skip . 0)
                     \number
                     #(map (lambda (x)
                             #{ \markup {
                               % create a column from one input sublist
                               \center-column #(map number->string x)
                               % process the hyphen if requested
                               #(if hyphen
                                    (if (eq? x (last timesigs))
                                        ;; do not print a hyphen after the last column
                                        ""
                                        ;; generate the hyphen
                                        ; TODO: make appearance configurable
                                        (markup
                                         #:line
                                         (#:hspace -0.25
                                           (#:override
                                            (cons (quote thickness) 3.4)
                                            (#:draw-line (cons 0.9 0)))
                                           #:hspace -0.15)))
                                    "")
                                }
                             #}) used-signatures)
                   #}))
               (ly:input-message location (_i "Error in \\fractionList.
 Please use time signatures with two elements.")))))))

% This is a function to make it more accessible in standard cases
alternatingTimeSignatures =
#(define-music-function (parser location timesigs) (list?)
   (let* (;; code for stripping off hyphen variable has to be copied
           (hyphen (and (boolean? (car timesigs))
                        (car timesigs)))
           (used-signatures (if hyphen
                                (cdr timesigs)
                                timesigs))
           ;; determine the first time signature from the list
           (first-effective-timesig
            (cons
             (caar used-signatures)
             (cadar used-signatures))))
     ;; override the stencil and apply the first time signature
     #{
       \once \override Score.TimeSignature.stencil = \fractionList #timesigs
       \time #first-effective-timesig
     #}))
