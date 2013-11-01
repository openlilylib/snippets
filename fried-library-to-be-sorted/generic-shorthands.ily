\version "2.17.3"

% /includes/ulLibrary/tweaks/grobSpecificTweaks.ily

%{
  Move note heads right or left to avoid
  collisions or NoteColumn clashes
  Use of \moveNote is preferred over manual tweak of #'force-hshift
  because the tweak is colored in draftMode
%}

moveNote =
#(define-music-function (parser location amount)
   (number?)
   #{
     \once \override NoteColumn #'force-hshift = #amount
   #})


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move a rest up or down when LilyPond doesn't do the right thing

moveRest =
#(define-music-function (parser location staff-position)
   (integer?)
   #{
     \once \override Rest #'staff-position = #staff-position
     \once \override MultiMeasureRest #'staff-position = #staff-position
   #})


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beampos =
#(define-music-function (parser location pos)
   (pair?)
   #{
     \once \offset Beam #'positions #pos
   #})

staffdist =
#(define-music-function (parser location distances)
   (list?)
   #{
     \overrideProperty #"Score.NonMusicalPaperColumn"
     #'line-break-system-details
     #(list (cons 'alignment-distances distances))
   #})

tieconf =
#(define-music-function (parser location conf)
   (list?)
   #{ \once \override TieColumn #'tie-configuration = #conf #})

extraoff =
#(define-music-function (parser location val mus)
   (number-pair? ly:music?)
   #{
     \tweak #'extra-offset #val #mus
   #})

stemlen =
#(define-music-function (parser location val)
   (number?)
   #{
     \once \override Stem #'length = #val
   #})

padding =
#(define-music-function (parser location grob padding)
   (string? number?)
   #{
     \once \override $grob #'padding = #padding
   #})

tweakPadding =
#(define-music-function (parser location padding mus)
   (number? ly:music?)
   #{
     \tweak #'padding #padding #mus
   #})

positions =
#(define-music-function (parser location grob positions)
   (string? pair?)
   #{
     \once \override $grob #'positions = #positions
   #})

whiteout =
#(define-music-function (parser location mus)
   (ly:music?)
   #{
     \tweak #'whiteout ##t #mus
   #})

xoff =
#(define-music-function (parser location val mus)
   (number? ly:music?)
   #{
     \tweak #'X-offset #val #mus
   #})

yoff =
#(define-music-function (parser location val mus)
   (number? ly:music?)
   #{
     \tweak #'Y-offset #val #mus
   #})

xyoff =
#(define-music-function (parser location xval yval mus)
   (number? number? ly:music?)
   #{
     \tweak #'X-offset #xval \tweak #'Y-offset #yval #mus
   #})
