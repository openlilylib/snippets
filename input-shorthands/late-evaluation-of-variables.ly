\version "2.16.2" % absolutely necessary!

% use this to crop output page size to match the snippet
#(ly:set-option 'preview #t)

\header {
  snippet-title = "Late evaluation of variables"
  snippet-author = "Kristóf Marussy"
  % taken from https://gist.github.com/kris7topher/6038247
  % featured on Lilypond blog (provide reference)9
  snippet-description = \markup {
    “Late-bind” variables using an arity-0 music function that looks up
    values at function call time instead of variable assignment time.
  }
  status = "unfinished"
  % TODO: tell Kristof about the snippet, reorganize description
  % add comma-separated tags to make searching more effective:
  tags = "arity-0, score organization, variable, transposition, lilypond blog"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

beginning = \relative c'' {
  \key as \major
  c1
}

middlesection = \relative gis' {
  \key e \major
  gis1
}

end = \relative c'' {
  \key as \major
  c1
  \bar "|."
}

% ``Late-bind'' variables using an arity-0 music function that looks up values
% at function call time instead of variable assignment time.
late =
#(define-scheme-function
  (parser location embedded-lilypond)
  (list?)
  (define-music-function
   (parser location)
   ()
   (eval embedded-lilypond
     (interaction-environment))))


music = \late #'#{
  \beginning
  \middlesection
  \end
#}

\score {
  % The music without the enharmonic change---complex key signature!
  \new Staff \transpose as g \music
  \layout {}
}

% Apply enharmonic transposition to \middlesection.
middlesection = \transpose e fes \middlesection

\score {
  % Because variables in \music are ``late-bound'', the enharmonic
  % transposition is now respected.
  % The point is that the reassignment of middlesection variable
  % is respected despite the fact that \music was defined earlier!
  \new Staff \transpose as g \music
  \layout {}
}
