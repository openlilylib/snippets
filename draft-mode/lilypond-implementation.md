## Implement a 'draft mode' in LilyPond

We intend to implement a draft mode in LilyPond itself, during first tests I made some observations and had ideas about a strategy.

- insert a check for the presence of a command line option `-ddraft-mode`  
  This check has to be inserted at a (still-to-be-found) point 
  where the input file(s) have already been parsed in order 
  to see options that have been set in the input file 
  with `#(ly:set-option '...)`
- if `-ddraft-mode` is present include a file `draft-mode-options.ly`  
  (see [draft-mode-options.ly](draft-mode-options.ly) for a draft).  
  This file checks for the presence of specific options and
  activates the respective functionality by either including 
  secondary 'style files' or setting options
  (e.g. `#(ly:set-option 'debug-skylines)`  
  Current ideas would include the following options:
    - `-ddraft-debug-skylines`
    - `-ddraft-display-controlpoints`
    - `-ddraft-color-voices`
    - `-ddraft-display-anchors`
    - `-ddraft-display-offsets`  
     (drawing a line from the default to the actual anchor)
    - `-ddraft-custom=`  
     (include a custom style file. I would for example have
      a `draft-editorial.ily` file to color editorial annotations
      as we have in the Fried edition)

- define all known/accepted options in `lily/program-option-scheme.cc`  
  in order to get rid of warnings about unknown internal options.

I think we should clearly differentiate between draft mode options 
that can be simply be switched on and off and options that require 
tedious implementation.
For example `debug-skylines` can be switched, the same is true for 
things like `display-control-points` and similar commands.
But there could be other ideas that require to override lots of
predefined commands (such as coloring objects, which might require
individual tweaks for all kinds of objects).  
But I'm convinced that we could implement the draft-mode as a feature
proposal quite quickly if we restrict ourselves to the 'switchable'
modes.
I think these already give a striking impression.
Once that's accepted with LilyPond it would be rather straightforward 
to extend with new features.

I further think that all functions that are used to format draft mode
items (like `\displayControlPoints`) have to be included to LilyPond
separately (thus: before the draft-mode) because they are needed for
draft mode, and because they are useful in themselves.