\version "2.18.0"

%{
  Example file for ScholarLY's \annotate functionality.
  This file contains a working example to show a number of features
  of the \annotate package. Play around with the commented configuration
  options and see what they do. The example is also used in the Wiki.
%}

\include "scholarly/annotate.ily"

% By default annotations are printed to the console.
% Uncomment the following line to deactivate this
%\printAnnotations ##f

% Activate output of annotations for both plaintext and latex
\setAnnotationExportTargets #'("plaintext" "latex")

% Optionally format timing information using the lilyglyphs LaTeX package
%\useLilyglyphsForBeatString ##t

% By default annotations are colored according to their annotation type
% Uncomment the following line to deactivate this
%\colorAnnotations ##f

% By default annotations are sorted by their rhythmic location
%\sortAnnotationsBy #'("type")

% Customize labelling of affected context
\addAnnotationContextLabels
#'(("left-hand" . "Linke Hand"))

% Configure a custom annotation type by setting some values:

\setAnnotationTypeLabel "custom-annotation" "Custom Annotation:"
\setAnnotationTypeColor "custom-annotation" #darkcyan

% Support custom properties by adding a label
\setAnnotationPropertyLabel "original-pitch" "Original Pitch"

% Arbitrary functions can be called to generate property values.
% It is possible to use define-scheme-function or define-music-function, but the
% function has to return a valid data type for use as property value
% TODO: Can markup functions be used too?
plus =
#(define-scheme-function (parser location a b)(number? number?)
   (format "~a" (+ a b)))

musicOne = \relative c' {
  r16 c d e  f
    \musicalIssue \with {
      author = "Foo Bar"
      message = "Could this be dis?"
    }
    NoteHead
    d e c  g'8 c
      \lilypondIssue \with {
        message = "This should be bigger"
      }
      Script
      b \prall c
}

dynamics =  {
  s2 r8
    \musicalIssue \with {
      message = "This hairpin is completely unoriginal"
    }
    Hairpin
    r4 \> r8 \!
}

musicTwo = \relative c {
  \clef bass
  \annotation \with {
    type = "custom-annotation"
    % uncomment the following to override the default value
    %context = "All instruments"
    message = "See \what is @\emph{possible}@"
    number-property = 12.3
    original-pitch = cis
    original-music = { c d e-> f }
    ossia = \score { \new Staff { c e d } }
    original-time = \time 3/4
    original-key = \key a \major
    arbitrary-scheme-list = #'(1 34 (3 . 2) 'foo)
    arbitrary-calulation = \plus 124 34
    original-text = \lyricmode {
      Dies ist ein Lied
    }
  } Staff.Clef
  r2 r16 c d e
    \question \with {
      message = "Which kind of beaming should we use?"
    }
    Beam
    f d e c
}



\score {
  \new PianoStaff
  <<
    \new Staff = "right-hand" \musicOne % here the staff name is used as context
    \new Dynamics \dynamics		% no name given => use input directory
    \new Staff = "left-hand" \musicTwo  % this is overridden through a command
  >>
  \layout {}
}