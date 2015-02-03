# ScholarLY

ScholarLY is a toolbox for scholarly editions with (mainly) GNU LilyPond.
Its main part is an includable LilyPond library which is complemented by LaTeX
tools and Python scripts.

## Installation

###LilyPond library 

There are two steps to "install" the LilyPond library once it is downloaded.
The first step is to meet the dependency to `openLilyLib`, which `ScholarLY` relies
on. Go to its [home page](https://github.com/openlilylib/openlilylib) and follow
the directions to get it to run.
Then simply add the `/ly` directory to LilyPond's include path, either through
supplying the respective command line option or by adding the path in Frescobaldi's
preferences. From there on you can use `scholarly` as a prefix to any include command
in LilyPond input files. This acts as a namespace to avoid name clashes with other
libraries or built-in commands. You can include individual functions, a number of
function collections or the full library at once.
Some examples:

- `\include "scholarly/full.ily"`  
  to include the complete library (not implemented yet)
- `\include "scholarly/annotate.ily"`  
  to include only the annotation functionality
- `\include "scholarly/items/diplomatic-line-breaks.ily"`  
  (function to indicate line breaks in the source, not implemented yet,
   naming subject to change)

