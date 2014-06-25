Merge Rests Engraver
====================

`merge-rests-engraver` and `merge-mmrests-engraver` are used to combine two rests of the same duration from two voices into one.  This eliminates the need to add `\once \omit` to the rest in one of the voices.  `merge-mmrests-engraver` combines whole measure and multi-measure rests and `merge-rests-engraver` combines all others.

Usage
-----

Add one or both engravers to the staff context in the layout section.

    \layout {
      \context { \Staff \consists #merge-rests-engraver }
      \context { \Staff \consists #merge-mmrests-engraver }
    }

See also `example.ly`.