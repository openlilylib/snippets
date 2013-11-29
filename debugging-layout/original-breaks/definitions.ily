\version "2.16.2"

\header {
  snippet-title = "Keep Original Breaks"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    This snippet adds commands to insert conditional breaks
    in a score, usually used to indicate breaks in the model
    a score is copied from. This is useful when entering or
    when proof-reading the score because the page layout of
    model and new score is the same.
    
    Available commands are:
    % generic
    - originalBreak
    - originalPageBreak
    - originalPageTurn
    % for breaks in the score
    - originalScoreBreak
    - originalScorePageBreak
    - originalScorePageTurn
    %for breaks in parts
    - originalPartBreak
    - originalPartPageBreak
    - originalPartPageTurn
    When the Scheme variable 'layout-keep-original-breaks'
    is defined and true these commands insert the corresponding
    break commands, otherwise they do nothing.
    This makes it possible to have the original breaking
    entered in the input files but only respected when the
    switch (the variable) is set.
    #(define layout-keep-original-breaks #t) activates original breaks,
    with #(define layout-keep-original-breaks #f) it is possible
    to deactivate original breaks, even during a score.
    
    %TODO: The following doesn't work yet.
    % It seems that redefining commands like \break
    % results in a crash ("parsing... Exited with exit status 1")
    Additionally this file redefines the standard commands
    - break
    - PageBreak
    - PageTurn
    When the Scheme variable 'layout-keep-only-original-breaks'
    is defined these commands do nothing, otherwise they
    insert the corresponding break commands.
    This functionality is useful in conjunction with the
    new commands in case you have already entered
    manual breaks that interfere with the "original"
    breaks.
  }
  % add comma-separated tags to make searching more effective:
  tags = "page layout, breaking"
  % is this snippet ready?  See meta/status-values.md
  status = "buggy"
}


%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
% A) generic functions

originalBreak =
#(define-music-function (parser location)()
   (_i "Line break in the original model. Forces a line break
     if the -dkeep-original-breaks option is set on the command line.")
   (if (and (defined? 'layout-keep-original-breaks)
            (eq? layout-keep-original-breaks #t))
        #{ \break #}
        #{ #}))

originalPageBreak =
#(define-music-function (parser location)()
   (_i "Page break of the original model. Forces a page break
     if -dkeep-original-breaks option is set on the command-line.")
   (if (and (defined? 'layout-keep-original-breaks)
            (eq? layout-keep-original-breaks #t))
        #{ \pageBreak #}
        #{ #}))

originalPageTurn =
#(define-music-function (parser location)()
   (_i "Page turn of the original model. Forces a page turn
     if -dkeep-original-breaks option is set on the command line.")
   (if (and (defined? 'layout-keep-original-breaks)
            (eq? layout-keep-original-breaks #t))
        #{ \pageTurn #}
        #{ #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B) functions for the score

originalScoreBreak =
#(define-music-function (parser location)()
   (_i "Line break in the original model. Forces a line break
     if the -dkeep-original-breaks option is set on the command line.")
   (if (defined? 'layout-keep-original-breaks)
        #{ \break #}
        #{ #}))

originalScorePageBreak =
#(define-music-function (parser location)()
   (_i "Page break of the original model. Forces a page break
     if -dkeep-original-breaks option is set on the command-line.")
   (if (defined? 'layout-keep-original-breaks)
        #{ \pageBreak #}
        #{ #}))

originalScorePageTurn =
#(define-music-function (parser location)()
   (_i "Page turn of the original model. Forces a page turn
     if -dkeep-original-breaks option is set on the command line.")
   (if (defined? 'layout-keep-original-breaks)
        #{ \pageTurn #}
        #{ #}))

%%%%%%%%%%%%%%%%%%%%%%%%
% C) functions for parts

originalPartBreak =
#(define-music-function (parser location)()
   (_i "Line break in a part of the original model. Forces a line break
     if the -dkeep-original-breaks option is set on the command line.")
   (if (defined? 'layout-keep-original-breaks)
        #{ \break #}
        #{ #}))

originalPartPageBreak =
#(define-music-function (parser location)()
   (_i "Page break in a part of the original model. Forces a page break
     if -dkeep-original-breaks option is set on the command-line.")
   (if (defined? 'layout-keep-original-breaks)
        #{ \pageBreak #}
        #{ #}))

originalPartPageTurn =
#(define-music-function (parser location)()
   (_i "Page turn in a part of the original model. Forces a page turn
     if -dkeep-original-breaks option is set on the command line.")
   (if (defined? 'layout-keep-original-breaks)
        #{ \pageTurn #}
        #{ #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% D) Only keep "original" breaks
%{
break =
#(define-music-function (parser location)()
   (_i "Line break in the original model. Forces a line break
     if the -dkeep-original-breaks option is set on the command line.")
   (if (defined? 'layout-keep-only-original-breaks)
        #{ #}
        #{ \break #}))


pageBreak =
#(define-music-function (parser location)()
   (_i "Page break of the original model. Forces a page break
     if -dkeep-original-breaks option is set on the command-line.")
   (if (defined? 'layout-keep-only-original-breaks)
        #{ #}
        #{ \pageBreak #}))

pageTurn =
#(define-music-function (parser location)()
   (_i "Page turn of the original model. Forces a page turn
     if -dkeep-original-breaks option is set on the command line.")
   (if (defined? 'layout-keep-only-original-breaks)
        #{ #}
        #{ \pageTurn #}))
%}