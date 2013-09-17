\version "2.17.25"

\header {
  snippet-title = "Adjusting horizontal spacing"
  snippet-author = "David Nalesnik"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "horizontal spacing, common shortest duration"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
  %{
    TODO:
    - change name - \horizontalSpacingDensity ?
    - make it possible to make spacing tighter (as of now,
    \scaleBaseShortestDurationFromDefault #(ly:make-moment 4 1)
    didn't do anything)
    - change the argument from a moment to a number?  So that
    one would write \horizontalSpacingDensity #2 and that 2
    would be internally translated into a moment
    - add derived commands \horizontalSpacingLoose and
    \horizontalSpacingTight
  %}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

scaleBaseShortestDurationFromDefault =
#(define-music-function (parser location factor)
   (ly:moment?)
   #{
     \override Score.SpacingSpanner.base-shortest-duration =
     #(lambda (grob)
        (let ((base-shortest-duration
               (assoc-get 'base-shortest-duration
                 (reverse (ly:grob-basic-properties grob)))))
          (ly:moment-mul base-shortest-duration factor)))
   #})


%%% moment offset test

#(set-global-staff-size 15)
\paper {
  indent = 0
  ragged-right = ##t
}

music = \relative c {
  \clef "bass"
  \key d \minor
  \time 3/4
  \mergeDifferentlyDottedOn
  <<
    { \slurDashed d8.-\flageolet( e16) e4.-\trill( d16 e) }
    \\
    { d4_2 a2 }
  >>
  \slurDashed
  <f' a, d,>4. e8( d c)
  \slurSolid
  bes8 g' f e16( f g_1 a_2 bes_3 d,_2)
}

\new Staff {
  \music
}

\new Staff {
  \scaleBaseShortestDurationFromDefault #(ly:make-moment 1 4)
  \music
}

\new Staff {
  \scaleBaseShortestDurationFromDefault #(ly:make-moment 1 8)
  \music
}
