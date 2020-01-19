% Tweaks that are global to make bravura look better

thinnerLedgerLines = {
    \override StaffSymbol.ledger-line-thickness = #'(1 . 0.06)
}

thinnerStems = {
    \override Stem.thickness = #0.92
}

shiftStems = { 
    \override Stem.X-offset = #(lambda (grob)
    (if (= UP (ly:grob-property grob 'direction))
        1.1
        0.06))
}

%Easy way to toggle bravura
bravuraToggle = {
	\bravuraOn
    \thinnerLedgerLines
    \thinnerStems
	\shiftStems
}