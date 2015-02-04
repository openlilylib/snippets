
%{
  This files contains general-purpose predicates for use with LilyPond and openLilylib
%}

% String list predicate
#(define (stringlist? obj)
   "Evaulates to #t when obj is a list containing exclusively of strings."
   (and (list? obj)
        (every string? obj)))

