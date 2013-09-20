## Ideas for adding a 'draft-mode' to LilyPond

In Frescobaldi, there are two "modes" of compiling a file:

* preview mode
* publication mode

The idea is that when you are working on the score, you use preview mode,
and when you're done, you use publication mode to get the "final" pdf.
Currently the only difference between these two modes is point-and-click -
preview mode has point-and-click turned on by default, while publication
mode has it turned off (you don't want these links in published scores,
after all, and they bloat the file size anyway).

Now, there are many more things that could be additionally displayed in
preview mode to help editing the score:

- control points of slurs, ties etc. could be displayed
- grob reference points (anchors) could be displayed
- skylines could be displayed (debug-skylines option)
- objects could be colored when they have a specific direction set  
  (so that it would be easy to see if the notes are for example set
  as a \voiceOne)
- tweaked objects could be highlighted
- a line from the original position to the current position could
  be shown  
 (for any tweaked objects)
- etc. (add your ideas here)

Additionally, the pdf created in preview mode could be saved under a 
differrent filename, so that one would have both "preview pdf" and
"final pdf" available at the same time (to save the time needed for
recompilation).

Note that this doesn't have to be a Frescobaldi-only feature, it could
be added to LilyPond itself.

In Frescobaldi it can be implemented by inserting options in the command line.
Finally one file is included which contains a function that includes more 
style files depending on the absence/presence of these command line options.

For ideas how to implement it in LilyPond itself see [lilypond-implementation.md](lilypond-implementation.md).
