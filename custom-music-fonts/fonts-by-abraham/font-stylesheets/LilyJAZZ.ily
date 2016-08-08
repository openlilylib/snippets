%%%% The stylesheet for LilyJAZZ music font and LilyJAZZText text font
%%%%
%%%% In order for this to work, this file's directory needs to 
%%%% be placed in LilyPond's path
%%%%
%%%% NOTE: If a change in the staff-size is needed, include
%%%% this file after it, like:
%%%%
%%%% #(set-global-staff-size 17)
%%%% \include "LilyJAZZ.ily"
%%%%
%%%% Copyright (C) 2014 Abraham Lee (tisimst@gmail.com)

\version "2.18.2"

% The following block requires the customized "font.scm" file in order to work
\paper {
  #(define fonts
     (make-pango-font-tree
      "lilyjazz"
      "emmentaler"
      "lilyjazztext"
      "sans"
      "monospace"
      (/ staff-height pt 20)))
}

% This is used to allow for local use of LilyJAZZ 
% (i.e., \override Staff.NoteHead.font-family = 'lilyjazz)
\include "font-register.ily"

\layout {
  \override Staff.Tie.thickness = #3
  \override Staff.Slur.thickness = #3
  \override Staff.PhrasingSlur.thickness = #3
  \override Score.Hairpin.thickness = #2
  \override Score.Stem.thickness = #2
  \override Score.TupletBracket.thickness = #2
  \override Staff.BarLine.hair-thickness = #2
  \override Staff.BarLine.thick-thickness = #4
  \override Staff.MultiMeasureRest.hair-thickness = #3
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start with a repeat Barline
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showStartRepeatBar = { 
  \once \override Score.BreakAlignment.break-align-orders =
        #(make-vector 3 '(instrument-name
                          left-edge
                          ambitus
                          breathing-sign
                          clef
                          key-signature
                          time-signature
                          staff-bar
                          custos))
      \once \override Staff.TimeSignature.space-alist =
        #'((first-note . (fixed-space . 2.0))
           (right-edge . (extra-space . 0.5))
           ;; free up some space between time signature
           ;; and repeat bar line
           (staff-bar . (extra-space . 1)))
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating jazz-style repeats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%=> http://lsr.di.unimi.it/LSR/Item?id=753
#(define (white-under grob) (grob-interpret-markup grob 
  (markup #:vcenter #:whiteout #:pad-x 1 (ly:grob-property grob 'text))))

inlineMMR = {
  \once \override MultiMeasureRest.layer = #-2
  \once \override MultiMeasureRestNumber.layer = #-1
  \once \override MultiMeasureRestNumber.Y-offset = #0
  \once \override MultiMeasureRestNumber.stencil = #white-under
  \once \override MultiMeasureRest.rotation = #'(2 0 0)
}


