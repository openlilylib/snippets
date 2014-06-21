*shapeTieColumn* Function
=======================

This function can adjust each individual tie in a chord using a `\shape`-like syntax.  

	\shapeTieColumn #'( ( <chord_tie_1> ) ( <chord_tie_2> ) ... )
    chord_tie_n = ( <shape_1> [<shape_2>] )
    shape_n = ( <start_point> <left_bend_point> <right_bend_point> <end_point> ) | ()
    xxx_point = ( <X_column> . <Y_column> )

The function takes a list of lists.  The first list (`chord_tie_1`) controls the tie for the first note in the chord.  For example, if the chord is `<c e g>`, then `chord_tie_1` controls the note C.

Each `chord_tie` takes one or two lists.  If there is no break between the starting chord and the final chord (unbroken), only one `shape` is needed.  This `shape` will control one tie between two notes.  If the tie is broken, `shape_1` will control the tie before the break and `shape_2` will control the tie after.

Each `shape_n` has the syntax of `\shape`: four pairs of numbers that represent the X and Y of a point.  The first is the starting point; the second, the point of the left bend in the curve; the third, the right bend; and the fourth, the ending point.  The empty list, `()` is shorthand for no change. 

See also [the original message.](http://www.mail-archive.com/lilypond-user%40gnu.org/msg89506.html) and [the syntax for \shape.](http://lilypond.org/doc/v2.18/Documentation/notation/modifying-shapes)
