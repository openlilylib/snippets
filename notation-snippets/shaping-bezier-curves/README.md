Improved versions of slur shaping function
------------------------------------------

Here are some experimental functions that allow to shape slurs more efficiently
than the `\shape` currently included in LilyPond.  Please see example files for
more information.


### TODOs:

() should always result in that point not being modified (in polar coords this is not equivalent to (0 . 0))

it should be possible to turn detecting curve direction off

make sure specyfying coords relative to noteheads works well in case of ties as well.

modes:
- absolute values
- offset against default values
- (for outer pts) relative to the notehead
- (for inner pts) relative to outer pts
- polar coords
- relative polar coords (i.e. angles relative to the line connecting outer ctrpts. make sure it works with different dirs and slopes)
- polar relative to default
- last point relative to first (so that there is a certain height difference, or slope)

() for the whole sibling should always mean "unchnaged" (polar has some problems with that)

It's connfusing when signs change like this. (mirrored when using ((a b)(c d)), not mirrored with ((a b)(c d)(a b)(c d))

flipping polar's outer y (when dir = DOWN) doesn't work

Addition for displaying controlpoints - display coordinates of each point, maybe in the format used? (i.e. polar, offset, absolute...)


Slur attachment:
detect the case when there's no stem on the side of the notehead where the slur is attached (whole notes, or Kieren example) and do a default attachment then.
If just one attachment is specified, duplicate it.

### BUGS:

polar doesn't handle () well (at least for inner coords)
