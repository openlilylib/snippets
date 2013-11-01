\version "2.17.3"
% /includes/ulLibrary/curvesToolbox/shapingCurves.ily

%{
  This file is part of ulLibrary (Urs Liska's LilyPond Toolbox)
  but contains functions which are mainly the work of
  David Nalesnik (david.nalesnik@gmail.com)
  %TODO: Search for the other contributors
  
  Basically it exports the functions
  - shapeSlur
  - shapePhrasingSlur
  - shapeTie
  They manipulate the respective curves through defining
  offsets to LilyPond's default decisions (instead of having to find
  hard-coded coordinates. This has the main advantage that there is 
  a good chance that the override also works when the layout changes.
  
  There are several additions to an earlier version available at
  the LilyPond Snippet Repository:
  - The function now handles slurs that are broken into more than one snippet ('siblings')
  - Any sibling can be shaped individually or left alone (i.e. it uses LilyPond's default)
  - If the number of siblings doesn't match the number of arguments
    (due to wrong input or changed layout)
    the function issues an informative warning including the reference to the input source
    and colors the respective curve so it can be easily found in the score
  - the used color can be personalized through a variable that can either be set
    in the library file or at any given moment in the music input
    (leaving it set to default black makes it seem there is no coloring)
    
  Usage:
  Call \shapeXXX immediately before the curve you want to modify.
  A set of control-point offsets is given as a list of four pairs, 
  each specifying x and y offsets ((x1 . y1) (x2 . y2) (x3 . y3) (x4 . y4))
  for example 
    \shapeSlur #'( ( ( 0 . 0 ) ( 5 . 7 ) ( 0 . -15 ) (0 . -4) ) ) )
  keep in mind: You need one pair of brackets around the whole function,
                         one pair of brackets around the set of control-points and
                         one pair around each control-point
  You have to take care to give exactly four points. 
  If you give less or more pairs you won't see warnings or errors  but the curves will be very strange.
  Giving no point at all is equivalent to ( (0 . 0) (0 . 0) (0 . 0) (0 . 0) )
  
  For more than one curve sibling enter the appropriate number of sets of control-points
  for example
    \shapeSlur #'( ( ( 0 . 0 ) ( 5 . 7 ) ( 0 . -15 ) (0 . -4) ) 
                   ( ( 3 . 0 ) ( 3 . 2 ) ( 12 . 15 ) (0 . -4) ) 
                   ()
                 )
    (In this example the third sibling is left its default control-points)
  
  If the number of control-points sets doesn't match the actual number of siblings
  the curve is broken into the function does two things:
  - issue an informative warning about the number of expected and existent siblings
    as well as pointing to the line of the music input
  - color the offending curve in the score so it can easily be spotted
  This is especially useful when the mismatch isn't the result of wrong input but 
  of a change in layout
  You can personalize the color with the variable curve-warning-color.
  Here are a few possibilities to set this variable:
    #(define curve-warning-color (rgb-color 0 0 1))
    #(define curve-warning-color (x11-color 'green))
    #(define curve-warning-color magenta)
  The default is black.
  You can either change the color within this file,
  anywhere in your file structure or
  anywhere in your source files (which allows to change the color during a piece
    (whatever this may be useful for ...))
    
  For usage examples see the lower part of this file.
%}



#(define curve-warning-color magenta)

#(define ((offset-control-points offsets function) grob)
   (let ((coords (function grob)))
     (if (null? offsets)
         coords
         (map
          (lambda (x y)
            (coord-translate x y))
          coords offsets))))

#(define ((shape-curve offsets location) grob)
   (let* ((orig (ly:grob-original grob))
          (siblings (if (and (ly:grob? orig) (ly:spanner? grob))
                        (ly:spanner-broken-into orig) '() ))
          (total-found (length siblings))
          (function (assoc-get 'control-points
                      (reverse (ly:grob-basic-properties grob))))
          (grob-name
           (assoc-get 'name
             (assoc-get 'meta
               (ly:grob-basic-properties grob)))))

     (define (helper sibs offs)
       (if (and (eq? (car sibs) grob)
                (pair? offs))
           ((offset-control-points (car offs) function) grob)
           (if (pair? offs)
               (helper (cdr sibs) (cdr offs))
               ((offset-control-points '() function) grob))))

     ; standardize input so #'((dx1 . dy1) . . . )
     ; and #'( ((dx1 . dy1) . . . ) ) possible
     (if (not (list? (car offsets)))
         (set! offsets (list offsets)))

     ; warnings
     (if (not (= (length offsets) total-found))
         (if (zero? total-found)
             (if (pair? (cdr offsets))
                 (begin
                  (set! (ly:grob-property grob 'color) curve-warning-color)
                  (ly:input-warning location
                    "~a is unbroken, modifications for ~a pieces requested"
                    grob-name (length offsets))))
             (if (eq? (last siblings) grob) ; print warning only once
                 (begin
                  (for-each
                   (lambda (piece) (set! (ly:grob-property piece 'color) curve-warning-color))
                   siblings)
                  (ly:input-warning location
                    "~a is broken into ~a pieces, modifications for ~a requested"
                    grob-name total-found (length offsets))))))

     (if (>= total-found 2)
         (helper siblings offsets)
         ((offset-control-points (car offsets) function) grob))))

% Commenting out our library version of \shape
% now every call to \shape uses LilyPond's built in \shape function.
% If it shows no problems we can remove the function alltogether

%{
shape =
#(define-music-function (parser location grob offsets)
                        (string? list?)
  #{
    \once \override $grob #'control-points =
      #(shape-curve offsets location)
  #})

%}

% The following three entry functions are deprecated
% and are kept only for compatibility with existing scores.

% For testing they are now commented out
% If everything works we should delete them

%{

shapeSlur = 
#(define-music-function (parser location offsets)
                        (list?)
  #{
    \once \override Slur #'control-points =  #(shape-curve offsets location)
  #})

shapePhrasingSlur = 
#(define-music-function (parser location offsets)
                        (list?)
  #{
    \once \override PhrasingSlur #'control-points = #(shape-curve offsets location)
  #})

shapeTie = 
#(define-music-function (parser location offsets)
                        (list?)
  #{
    \once \override Tie #'control-points = #(shape-curve offsets location)
  #})

%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Usage:

%{

%% SLURS

\markup {
  \underline "Default Slurs"
}

\relative c'' {
  d4( d' b g g,8 f' e d c2)
  \bar "||"
  d4( d' b g
  \break
  g,8 f' e d c2)
}

\markup {
  \underline "Modified"
}

\relative c'' {
  %% UNBROKEN
  %% remove semicolon to see warning
  \shapeSlur #'(
    ((0 . -2.5) (-1 . 3.5) (0 . 0) (0 . -2.5))
    ;()
    )
  d4( d' b g g,8  f' e d c2)
  \bar "||"

  %% BROKEN
  %% warning will show
  \shapeSlur #'(
    ((0 . -2.5) (0 . 1.5) (0 . 1) (0 . -1))
    ()
    ()
    )

  d4(^"(1st half only)" d' b g
  \break
  g,8 f' e d c2)
  \bar "||"

  %% both halves of the slur are modified
  \shapeSlur #'(
    ((0 . -2.5) (0 . 1.5) (0 . 1) (0 . -1))
    ((1 . 2) (0 . 1) (0 . 1) (0 . 0))
    )
  d4(^"(both halves)" d' b g
  \break
  g,8 f' e d c2)
}

%% TIES

\relative c'' {
  cis1~
  cis
  \shapeTie #'((0 . 0) (0 . 1) (0 . 1) (0 . 0))
  cis~
  cis
  \shapeTie #'((0 . 0) (0 . 1) (0 . 1) (0 . 0))
  cis~
  \break
  cis
  \break
  \shapeTie #'(
    ()
    ((-0.25 . 0) (0 . -0.25) (0 . -0.25) (0 . -1))
    )
  cis~
  \break
  cis
}

\paper {
  indent = 0
  ragged-right = ##t
}






%}