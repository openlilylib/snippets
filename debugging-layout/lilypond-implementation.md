## Implement Layout Debugging in LilyPond

We intend to implement a LilyPond mode to visualize aspects of the layout
to make debugging scores easier.

While we will start with a reference implementation in Frescobaldi
the ultimate goal is to add this to LilyPond itself,
so Frescobaldi development should always have this in mind
(with regards to namings, calling conventions etc.).

For an implementation in LilyPond we currently see the following strategy:

- insert a check for the presence of a command line option `-ddebug-layout`  
  This check has to be inserted at a (still-to-be-found) point 
  where the input file(s) have already been parsed in order 
  to see options that have been set in the input file 
  with `#(ly:set-option '...)`  
  Interesting files to investigate seem to be  
  - `scm/backend-library.scm`  
  - `scm/lily.scm` (with the main entry point)  
  - `ly/property-init.ly`
- if `-ddebug-layout` is present include a file `debug-layout-options.ly`  
  (see [debug-layout-options.ly](debug-layout-options.ly) for a draft).  
  This file first sets some default values we consider
  appropriate for a simple `-ddebug-layout`,  
  then checks for the presence of specific options and
  activates/deactivates the respective functionality by either  
  including secondary 'style files' or setting options
  (e.g. `#(ly:set-option 'debug-skylines)`  
  Current ideas would include the following options:
    - `-ddebug-skylines`  
      (this would imply modifying current behaviour:  
       `debug-skylines` would then only be active together with `debug-layout`)
    - `-ddebug-control-points`
    - `-ddebug-voices`
    - `-ddebug-anchors`
    - `-ddebug-offsets`  
     (drawing a line from the default to the actual anchor)
    - `-ddebug-custom=`  
     (include a custom style file. I would for example have
      a `draft-editorial.ily` file to color editorial annotations
      as we have in the Fried edition)

- there should be known options to configure the style of the debugging
  (colors, line-thickness etc.)- define all known/accepted options in `lily/program-option-scheme.cc`  
  in order to get rid of warnings about unknown internal options.
- Of course add documentation at all relevant places.

There are modes that can be achieved with a simple switch
by including single files (e.g. `\displayControlPoints`).
These modes usually rely on snippets that are useful in itself,
so they should be submitted to LilyPond not only in the context
of a `debug-layout` but also as standalone extensions.

The options will be developed firstly in Frescobaldi,
but we should concentrate on 'easy' ones first, because they
are already are extremely helpful.
