Slur-attachment function and improved `\shape`
----------------------------------------------

Here's a greatly expanded version of the `\shape` function, and another
function for specifying where slur ends should be attached to notes.

Please see `.pdf` files for more information.

Both functions are ready to be used, but they need testing;
they are not considered stable yet and their behaviour may change.

### TODOs for the future:

* Implement a mode for specifying control-point positions
relative to NoteColumn's Stem.

* Avoid collisions with Stems and NoteHeads when using 'head' mode.

* Implement a mode for specifying inner control-point positions
relative to outer points' positions.

* for displaying controlpoints - show coordinates of each point,
maybe in the format used? (i.e. polar, offset, absolute...)
