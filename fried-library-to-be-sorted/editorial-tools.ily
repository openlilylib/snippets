\version "2.17.3"
% /includes/ulLibrary/editorialToolbox/editorial.ily

%{
  This library is part of ulLibrary (Urs Liska's LilyPond Toolbox)
  and may also be included separately.
  It assumes that the project root contains any number of 
  first level subdirectories containing the to-be-compiled .ly files
  and a folder /includes/ulLibrary.
  So this file is always accessible through the relative path "../includes/ulLibrary/editorialToolbox/editorial.ily
  
  The editorial toolbox contains 
  functions to visualize editor's decisions and additions to the original music
  through the usual editorial means (printing smaller grobs, dashing, parenthesizing)
  
  TODO: This file is conceptually meant to cover the full range of grobs, 
  but is just at the beginning (implementing those objects that I've already encountered).
  
  This file is complemented by editorial_draftMode.ily
  which highlights editor's amendements during the editing process.
%}

\include "../includes/ulLibrary/draftMode_colors.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variable definitions for use throughout this file
% We define a few variables to provide coherence

% Font size for smaller items
#(define smF -3)

% Definition of the dash pattern 
edDashDefinition = #'(( 0 1 0.5 1.25 ))

%TODO: separate the 'productivity' tools and create a new file inSourceCommunication.ily

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Editorial productivity tools: comment, discuss, todo and poke

%{ \comment
   Post an editor's comment in the source file and attach it to a grob.
   Meant for communication between different editors of a file
   and for musically documenting the source file.
   
   Usage: \comment grob-name comment. Place immediately before the grob in question.
          \comment DynamicText "mf according to autograph" a\mf
  
   grob-name 
   is the name of the affected grob (works with or without quotation marks)
   comment 
   isn't used in the function itself, but only serves as the practical place for the comment,
   which is also consistent with \discuss and \todo, so the are interchangeable.   
   Should be a rather short string. If there is the need for a longer comment, it should 
   be entered as a regular (multi-line) comment and only referenced 
   in the function.
   
   It is basically a syntax for entering short comments,
   and it enables draftMode to color the corresponding grob
   with the default #editorial-remark-color
   In this file it is just a void music function,
   necessary to provide a coherent input syntax
%}
comment =
#(define-music-function (parser location grob comment)
   (string? string?)
   #{
     % issue void music expression
   #})

%{
  \discuss
  Post an editor's comment in the source file and attach it to a grob.
  It is very similar to \comment, but additionally issues a warning to the console.
  The comment parameter isn't used in the function because it is visible in the
  warning anyway. It should be used as a reminder for musical (or technical) issues that
  should still be taken care of.
  DraftMode also colors the grob with #editorial-remark-color.
  When the issue is dealt with, \discuss should be either removed or changed to \comment,
  but without draftMode it doesn't have any effect on the layout.
%}

discuss =
#(define-music-function (parser location grob comment)
   (string? string?)
   (ly:input-warning location "Editor's remark")
   #{
     % issue void music expression
   #})

%{
  \todo
  Post a comment in the source file and attach it to a grob.
  Meant for communication between different editors of the file

  Same as \discuss, but the act of coloring is hard-coded here and not left for draft mode 
  (while the color itself could be overridden in the source).
  This is because \todo indicates an issue that must be solved.
  So it should be also visible in pub mode to indicate that isn't solved yet.
  When the issue is solved \todo must be removed or renamed to \comment
%}

todo =
#(define-music-function (parser location grob comment)
   (string? string?)
   (ly:input-warning location "Editor's warning")
   #{
     \once \override $grob #'color = #todo-warning-color
   #})

%{
  \followup
  Post an editor's comment in the source file in reply to a comment
  entered through one of the preceding functions.
  
  Usage: \followup author comment
  
  Unlike these a \followup isn't attached to a grob, but issues a compiler warning.
  This can be used to comment on comments and see have an overview in the console window.
  It probably works best when placed at the beginning of a new line.
  the comment argument isn't used in the function but shown in the console as 
  the 'relevant' part of the input file
%}

followup =
#(define-music-function (parser location author comment)
   (string? string?)
   (ly:input-warning location author)
   #{
     % issue void music expression
   #})

%{ \poke
   Similar to Facebook's poke function
   this command is used to bring someone's attention to a speficic item
   
   Usage: \poke grob-name. Place immediately before the grob in question.
          \poke Slur ...
  
   grob-name 
   is the name of the affected grob (works with or without quotation marks)
   
   \poke enables draftMode to color the corresponding grob
   with the special #poke-color
   In this file it is just a void music function,
   necessary to provide a coherent input syntax
%}
poke =
#(define-music-function (parser location grob)
   (string?)
   #{
     % issue void music expression
   #})




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'Generic' additions by the editor
% These are considered deprecated, because - as there will be more definitions of grobs
% there will be more chances of several grobs starting at the same musical moment.
% So it is preferred to  define individual functions for different grobs.
% Additions by the Editor are marked by a smaller font-size

% beginEditorialAddition
bEdAdd = {
  \override NoteHead #'font-size = #smF
  \override Rest #'font-size = #smF
  \override Stem #'font-size = #smF
  \override Beam #'font-size = #smF
  %TODO: Are there more elements to resize?
}
% endEditorialAddition
eEdAdd = {
  \revert NoteHead #'font-size
  \revert Rest #'font-size
  \revert Stem #'font-size
  \revert Beam #'font-size
}
% single EditorialAddition
EdAdd = {
  \once \override NoteHead #'font-size = #smF
  \once \override Rest #'font-size = #smF
  \once \override Stem #'font-size = #smF
  \once \override Beam #'font-size = #smF
}

% Curves added by the editor are dashed
% We don't use the predefined commands to have more control over the dash definition
% and for draftMode to be able to color it

% Editorial Slur
edSlur = {
  \once \override Slur #'dash-definition = \edDashDefinition
  \once \override Slur #'thickness = #1.7
}

% Editorial PhrasingSlur
edPhrasingSlur = {
  \once \override PhrasingSlur #'dash-definition = \edDashDefinition
  \once \override PhrasingSlur #'thickness = #1.7
}

% Editorial Tie
edTie = {
  \once \override Tie #'dash-definition = \edDashDefinition
  \once \override Tie #'thickness = #1.7
}

%TODO: Add support for LaisserVIbrerTie and repeatTie


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Articulation

edArticulation =
#(define-music-function (parser location mus)
   (ly:music?)
   #{
     \parenthesize #mus
   #})


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lines

% editorialHairPin
edHP = {
  \once \override Hairpin #'style = #'dashed-line
}

% editorialStaffSwitch
edSS = {
  \once \override VoiceFollower #'style = #'dashed-line
  \once \override VoiceFollower #'dash-period = #1.8
  \once \override VoiceFollower #'dash-fraction = #0.5
}

% editorial arpeggio
% Place \edArpeggio immediately before the arpeggiated chord
% Function provided by David Nalesnik

edArpeggio= {
  \once \override Arpeggio #'stencil =
  #(lambda (grob)
     (parenthesize-stencil (ly:arpeggio::print grob) 0.1 0.5 0.5 0.2))
  \once \override Arpeggio #'before-line-breaking =
  #(lambda (grob)
     (set! (ly:grob-property grob 'X-extent)
           (ly:stencil-extent (ly:grob-property grob 'stencil) X)))
}

% editorial Fermata
% Place \edFermata immediately before the musical moment
% Works on \fermata as well as \fermataMarkup

edFermata = {
  \once \override Script #'font-size = #-2
  \once \override MultiMeasureRestText #'font-size = #-2
}

edTuplet = {
  \once \override TupletBracket #'style = #'dashed-line
}

% Editorial Stem
% As a Stem can't be dashed one has to attach a dashed line to it
% The music function has been provided by David Nalesnik
% and enhanced by Thomas Morley and David Kastrup
% The line width automatically reflects the original Stem's
% and the dash pattern can be given as arguments
% Note: It is in the caller's responsibility to make sure
% that the dashed stem's end matches the end of the stem
% (and isn't broken or ended with a gap)

#(define (make-round-filled-box x1 x2 y1 y2 blot-diameter)
   (ly:make-stencil (list 'round-filled-box (- x1) x2 (- y1) y2 blot-diameter)
     (cons x1 x2)
     (cons y1 y2)))

#(define (build-pos-list len on off)
   (let helper ((lst '()) (next 0) (on on) (off off))
     (if (< next len)
         (helper (cons next lst) (+ next on) off on)
         (reverse! lst (list len)))))

#(define (dashed-stem on off)
   (lambda (grob)
     (let* ((blot (ly:output-def-lookup (ly:grob-layout grob) 'blot-diameter))
            (stencil (ly:stem::print grob))
            (X-ext (ly:stencil-extent stencil X))
            (thickness (interval-length X-ext))
            (Y-ext (ly:stencil-extent stencil Y))
            (len (interval-length Y-ext))
            (new-stencil empty-stencil)
            (factors (build-pos-list len on off)))

       (define (helper args)
         (if (<= 2 (length args))
             (begin
              (set! new-stencil
                    (ly:stencil-add
                     new-stencil
                     (ly:stencil-translate-axis
                      (make-round-filled-box (/ thickness -2) (/ thickness 2)
                        (car args) (cadr args)
                        blot)
                      (interval-start Y-ext)
                      Y)))
              (helper (cddr args)))
             new-stencil))

       (if (or (zero? on) (zero? off))
           stencil
           (helper factors)))))

edStem =
#(define-music-function (parser location on off) (number? number?)
   #{
     \once \override Stem #'stencil = #(dashed-stem on off)
   #})
