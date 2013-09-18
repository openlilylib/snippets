\version "2.17.25"

\header {
  snippet-title = "Adjusting horizontal spacing"
  snippet-author = "David Nalesnik, Janek Warchol"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "horizontal spacing, common shortest duration"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
  %{
    TODO:
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

horizontalSpacingDensity =
#(define-music-function (parser location factor) (ly:moment?)
  (_i "This function determines the default value of the property
@var{common-shortest-duration} and multiplies it by the argument
@var{factor}.  Moments smaller than @code{(ly:make-moment 1 1)} make the spacing
tighter, while larger values make the spacing looser.
")
  #{
    \override Score.SpacingSpanner.common-shortest-duration =
      #(lambda (grob)
        (let* ((func (assoc-get 'common-shortest-duration
                                (reverse (ly:grob-basic-properties grob))))
               (default-value (func grob)))
          (ly:moment-mul default-value factor)))
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

% default horizontal spacing

\new Staff {
  \music
}

\new Staff {
  \horizontalSpacingDensity #(ly:make-moment 3 4)
  \music
}

\new Staff {
  \horizontalSpacingDensity #(ly:make-moment 1 2)
  \music
}

\new Staff {
  \horizontalSpacingDensity #(ly:make-moment 4 3)
  \music
}
