ScholarLY
=========

Roadmap for \annotate
---------------------

`\annotate` is a system to annotate scores in the input files. Annotations can be printed
to the console (providing clickable links) or exported to output files of various formats.
The first version is already impressive, but there's still a long way to go until it is a 
really versatile and robust solution.  
This document is not really a roadmap but rather a wish list, partially grouped by topics,
partially ordered by importance and/or implementation effort (that is, features that are
less important but trivial to add may be listed first).

### Technical foundation

- Convert to a Scheme module  
  This will be much cleaner, and it will allow to use much shorter names as the names are
  local to the module.
- Revise the structure of the annotation object  
  So far an annotation is a flat alist holding many "properties". It should have a number
  of "meta" properties that hold their own alists for properties that are never used to be
  printed. This will make the necessity obsolete to filter out so many properties in the
  export routines.  
  Maybe it is a good idea to have *only* such sub-alists as the top-level items.
- Properly thing about supporting custom annotation types
- Add support for callback functions dealing with custom property types
  
### Output processing

- Improve grouping options
- Output to different files (one for each annotation type)
- Make outputting annotations template based  
  Or in general think about how output can be made as configurable
  as possible.
- Improve handling of output file names.
- automatic line breaking for multiline properties (e.g. message)
- Is it possible to support musical notation in annotations (i.e. 
  inserting a `\score` block.  
  In some output formats such images could be used:  
  - LaTeX => PDF  
  - HTML => SVG or PNG
  - Support counting of annotations (to create cross-references)

#### LaTeX output

- incorporate `lilyglyphs`  
  Use it to (optionally) visualize the rhythmic location
- provide some sort of Markdown-like message formatting
- make an option to output standalone LaTeX documents

#### New output formats

- JSON
- Markdown
- HTML
- plain text with links (-> "Frescobaldi mode")

### New functionality

- Enable annotations to produce
  - footnotes
  - balloonText
  - ... ?  
  This should be triggered by additional properties, e.g. a `balloon-text` property would
  print its content as a ballon help or - when empty - the content of the message.  
  This could be the same with footnotes, although with footnotes it is much more
  usual to have a dedicated (short) text referencing the annotation in the appendix.
- Enable annotations to produce "editorial functions" and apply them to the affected grob.  
  This is a major goal but also one of the most promising ones. A critical remark can
  affect grobs in a persistent way beyond the temporary coloring. Of course such functions
  must be reliable and configurable (we can't prescribe how an edition should highlight 
  their operations).

### LaTeX package

- properly create a robust package that doesn't get in the way of others (i.e. only define
  what is really necessary to run it).
- Make the formatting of entries as modular and flexible as possible so users/projects can
  adapt them to their needs without fiddling around in the package itself.  
  I think most of the formatting should be done using macros that can individually be
  redefined or influenced by setting "properties"
  
### Frescobaldi integration

- Add Preference "Enable ScholarLY Support" and save the include path.
- interpret exported annotations and display them in a new "Annotation Browser".
- Annotation editor dialog
- Search and replace over annotations
- access annotation editor through context-menu on Music view (or through cursor position)
- Show tooltip with annotation content
