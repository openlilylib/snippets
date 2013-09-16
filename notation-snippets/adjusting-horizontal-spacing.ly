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

\new Staff {
  \scaleBaseShortestDurationFromDefault #(ly:make-moment 1 1)
  \repeat unfold 80
  {
    c''4
  }
}

\new Staff {
  \scaleBaseShortestDurationFromDefault #(ly:make-moment 1 4)
  \repeat unfold 80
  {
    c''4
  }
}

\new Staff {
  \scaleBaseShortestDurationFromDefault #(ly:make-moment 1 16)
  \repeat unfold 80
  {
    c''4
  }
}
