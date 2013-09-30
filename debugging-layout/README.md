Debugging lilypond layout
-------------------------

Sometimes you'd like to see why LilyPond did something, or you'd like
to see some details like the positions of slur control points.  This
category is for snippets like this.


### debugging layout and Frescobaldi

In Frescobaldi, there are two "modes" of compiling a file:

* preview mode
* publication mode

The idea is that when you are working on the score, you use preview mode,
and when you're done, you use publication mode to get the "final" pdf.
Currently the only difference between these two modes is point-and-click -
preview mode has point-and-click turned on by default, while publication
mode has it turned off.  
We think that preview mode is a great place to use layout-debugging snippets
from here.

We are working on a considerable extension of Frescobaldi's preview mode
with the intention of proposing a `-ddebug-layout` option for LilyPond
proper once it's sufficiently stable, and once 2.18 is released.  
The Frescobaldi implementation is developed in our fork on the
[debug-layout](https://github.com/openlilylib/frescobaldi/tree/debug-layout)
branch.
For more information see the [Wiki page]
(https://github.com/openlilylib/frescobaldi/wiki/Preview-Mode-Development-Roadmap).
