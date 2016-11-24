% Add standard midi support, preferably able to 
% switch instruments for different staves 
#(newline)
#(display "Midi Support loaded")

bassPitchOne		= #(ly:make-pitch -2 0 NATURAL)
bassPitchTwo		= #(ly:make-pitch -3 6 NATURAL)

sidePitchOne		= #(ly:make-pitch -2 2 DOUBLE-FLAT)
sidePitchTwo		= #(ly:make-pitch -2 1 NATURAL)

tenorPitchOne		= #(ly:make-pitch -2 3 NATURAL)
tenorPitchTwo		= #(ly:make-pitch -2 4 NATURAL)
tenorPitchThree		= #(ly:make-pitch -2 5 NATURAL)
tenorPitchFour		= #(ly:make-pitch -2 6 NATURAL)
tenorPitchFive		= #(ly:make-pitch -1 0 NATURAL)
tenorPitchSix		= #(ly:make-pitch -1 1 NATURAL)
tenorPitchSeven		= #(ly:make-pitch -2 3 NATURAL)
tenorPitchEight		= #(ly:make-pitch -2 3 NATURAL)
tenorPitchNine		= #(ly:make-pitch -2 3 NATURAL)
tenorPitchTen		= #(ly:make-pitch -2 3 NATURAL)
tenorPitchEleven	= #(ly:make-pitch -2 3 NATURAL)

% Default Pitches
sideDefault = \sidePitchTwo
bassDefault = \bassPitchTwo
tenorDefault = \tenorPitchSeven

midiDrumPitches.right-hand = \sideDefault
midiDrumPitches.left-hand = \sideDefault

\midi {
	\context {
		\DrumStaff
		\name PipeBandDrumStaff
		\alias DrumStaff

		% trying midi are you?
		midiInstrument = #"drums"

		drumPitchTable = #(alist->hash-table midiDrumPitches)
	}
	\context {
		\Score
		\accepts "PipeBandDrumStaff"
	}
	\context {
		\StaffGroup
		\accepts "PipeBandDrumStaff"
	}

}

%setMidiHands = 
%#(define-music-function
%	(parser location pitch)
%	(ly:pitch?)
%	(
%	 (display "Set Hands to pitch")
%	 #{
%		midiDrumPitches.right-hand = #pitch
%		midiDrumPitches.left-hand = #pitch
%
%		drumPitchTable = #(alist->hash-table midiDrumPitches)
%	 #}
%	)
%)
