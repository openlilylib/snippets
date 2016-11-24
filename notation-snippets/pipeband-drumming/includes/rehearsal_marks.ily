% =================================================	%
% 	Rehearsal Marks									%
% =================================================	%
#(newline)
#(display "Midi Support loaded")
% From Svenax's bagpipemusic
markText = #(define-music-function (parser location text) (string?) #{
    \once \override Score.RehearsalMark #'self-alignment-X = #LEFT
    \mark \markup $text
#})

markTextEol = #(define-music-function (parser location text) (string?) #{
    \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
    \mark \markup $text
#})

markTextEolDown = #(define-music-function (parser location text) (string?) #{
    \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
    \once \override Score.RehearsalMark #'direction = #DOWN
    \mark \markup $text
#})
altBracket = #(define-music-function (parser location tag) (string?) #{
		\set Score.repeatCommands = #(list (list 'volta (markup #:text tag)))
	#}
)


% Short al fine's and stuff
dacapoalfine = {\markTextEol "D.C. al fine" }

