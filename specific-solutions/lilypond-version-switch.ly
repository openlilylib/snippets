\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Switch Based on LilyPond Version"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    This snippet allows you to switch execution
    based on the currently run LilyPond version.
    This is useful if you want to provide general code
    (such as snippets or a library) but have to take
    changes in LilyPond syntax into account.
    
    The function takes a LilyPond version number (formatted
    as a list) as its argument and returns true if it is
    run by a LilyPond version greater than the argument.
    
    The recommended way to use this is to make a switch
    in the program flow within a music function as
    shown in the usage example.
  }
  % add comma-separated tags to make searching more effective:
  tags = "Program flow, LilyPond versions"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (calculate-version ver-list)
   ;; take a LilyPond version number as a list
   ;; and calculate a decimal representation
   (+ (first ver-list) (second ver-list) (third ver-list)))

#(define (lilypond-greater-than? ref-version)
   (> (calculate-version (ly:version))
      (calculate-version ref-version)))


%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%

%{
versionComment = 
#(define-music-function (parser location ver)
   (list?)
   (if (lilypond-greater-than? ver)
       #{ c'^\markup "Higher" #}
       #{ c'_\markup "Lower/Equal" #}))

{
  c'1^\markup "2.16.2"
  \versionComment #'(2 16 2)
}

{
  c'1^\markup "2.17.5"
  \versionComment #'(2 17 5)
}

%}