% ===================================================	%
% 	Layout tweaks for good defaults						%
% ===================================================	%
#(newline)
#(display "Layout tweaks loaded")
% Note name defs :
%	"d" for the right hand ("droite") and
%	"g" for the left hand ("gauche")
drumPitchNames =
#(append '(
	(d . right-hand)
	(g . left-hand)
	(right . right-hand)
	(left . left-hand)
	)
  drumPitchNames
)

% Position according to the line : right hand above and left hand below
#(define pipeband-style '(
	(right-hand	()	#f	1)
	(left-hand	()	#f	-1)
	)
)

\layout {
	indent = 0.0
	\context {
		\DrumStaff
		\name PipeBandDrumStaff
		\alias DrumStaff
		
		drumStyleTable = #(alist->hash-table pipeband-style)

		% one line per staff
		\override StaffSymbol.line-positions = #'(0)

		% bar line height
		\override BarLine.bar-extent = #'(-2 . 2)

		% stems
		\override Stem.direction = #DOWN		% stems down
		\override Stem.length = #8.5			% unbeamed stems length
		\override Stem.stemlet-length = #1	% short stem length

		% beams
		\override Beam.beam-thickness = #0.4		% beam-thickness
		\override Beam.positions = #'(-3.8 . -3.8)	% fix beams on one height

		% slurs and ties
		\override Slur.direction = #UP	% Slurs on top
		\override Tie.direction = #UP	% Ties on top

		% dynamics up
		\dynamicUp

		% slurs below rolls number
		%\override TextScript.outside-staff-priority = ##f
		%\override TextScript.side-axis = #0
		%\override TextScript.staff-padding = #3
		%\override TextScript.X-offset = #1				% padding to stems
		%\override TextScript.extra-offset = #'(-0.3 . 0)

		% tremolos (rolls)
		\override StemTremolo.slope = #0.5					% slope
		\override StemTremolo.beam-width = #1.5				% beam-width
		\override StemTremolo.beam-thickness = #0.3			% beam-thickness
		\override StemTremolo.extra-offset = #'(0 . 0.3)	% vertical pos. position

		\override TupletBracket.bracket-visibility = #'if-no-beams

		% unison brackets
		\consists "Horizontal_bracket_engraver"
		\override HorizontalBracket.staff-padding = #3.5		% staff-padding
		\override HorizontalBracket.direction = #UP			% brackets above the staff
		\override HorizontalBracket.bracket-flare = #'(0 . 0)	% vertical brackets

		subdivideBeams = ##t
		strictBeatBeaming = ##t
		\numericTimeSignature

		%Because it's funny
		\override Clef.stencil = #
			(lambda (grob)(grob-interpret-markup grob
				#{ \markup\combine
					\musicglyph #"clefs.percussion"
					\translate #'(2 . 0)
					\override #'(baseline-skip . 1) 
					\column {
					  "R"
					  "L"
					}
				#}
			))

	}
	\context {
		\Score
		\accepts "PipeBandDrumStaff"

		\override RehearsalMark.break-align-symbols = #'(clef)
		\override RehearsalMark.padding = #3
		\override VoltaBracket.edge-height = #'(1.5 . 1.5)
	}
	\context {
		\StaffGroup
		\accepts "PipeBandDrumStaff"
	}
}
