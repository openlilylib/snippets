% =================================================	%
% 	Side Sticking (modifies note, or adds mark)		%
% =================================================	%
% Part of lilydrum
backstick	= #(define-music-function (parser location notes) (ly:music?)
				#{
					\temporary \override Staff.NoteHead.style = #'cross
					$notes
					\revert Staff.NoteHead.style
				#})
crossstick	= #(define-music-function (parser location notes) (ly:music?)
				#{
					\temporary \override Staff.NoteHead.style = #'xcircle
					$notes
					\revert Staff.NoteHead.style
				#})
rimshot		= #(define-music-function (parser location notes) (ly:music?)
				#{
					$notes

				#})
