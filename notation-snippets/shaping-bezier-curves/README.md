Improved versions of slur shaping function
------------------------------------------

Here are some experimental functions that allow to shape slurs more efficiently
than the `\shape` currently included in LilyPond.  Please see example files for
more information.

The best part?  It's backward-compatible with the version of \shape in the current distribution!

### TODOs:

make sure specyfying coords relative to noteheads works well in case of ties as well.

modes to implement:
- (for outer pts) relative to the notehead
- (for inner pts) relative to outer pts
- relative polar coords (i.e. angles relative to the line connecting outer ctrpts. make sure it works with different dirs and slopes)
- polar relative to default
- last point relative to first (so that there is a certain height difference, or slope)

Addition for displaying controlpoints - display coordinates of each point, maybe in the format used? (i.e. polar, offset, absolute...)


Slur attachment:
detect the case when there's no stem on the side of the notehead where the slur is attached (whole notes, or Kieren example) and do a default attachment then.
If just one attachment is specified, duplicate it.
