Improved versions of slur shaping function
------------------------------------------

Here are some experimental functions that allow to shape slurs more efficiently
than the `\shape` currently included in LilyPond.  Please see example files for
more information.

The best part?  It's backward-compatible with the version of \shape in the current distribution!

### TODOs:

modes to implement:
- (for outer pts) relative to the notehead, stem or beam (?)
- (for inner pts) relative to outer pts

Addition for displaying controlpoints - display coordinates of each point, maybe in the format used? (i.e. polar, offset, absolute...)

add stem spec
avoid stems when using head spec