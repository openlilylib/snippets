
lalily scheme-modules
=====================

The core functions of [lalily](https://github.com/jpvoigt/lalily/) shall be separated and provided as independant modules,
but they share a lot of scheme-code. 

utilities
---------

Functions defined in module (scheme-lib lalily utilities):

* (base26 i) : Create a string [A-Z]+ from a positive integer.
  This is used to provide a letter-based counter for dot-notation.
* (glue-list l) : Combine list to one string like `string-join`, but accept any object type in the list.
* (glue-symbol l) : Combine list to one string and convert that to a symbol.
* (moment->string m) : Make a human-readable string from a moment.

storage
-------

Class types and functions to store objects:

* class `<stack>` : stack with methods
  * `(stack-create <name>)` : create stack with name
  * `(stack-push <stack> <obj>)` : push object on top of stack
  * `(stack-pop <stack>)` : get and remove top of stack
  * `(stack-get <stack>)` : get top of stack
* class <tree> : a directory-like store
  * `(tree-create <name>)` : create tree with name
  * `(tree-set! <tree> <path> <obj>)` : set value of <path> (a list) to obj
  * `(tree-get <tree> <path>)` : get value, stored at <path> or false



lalily
======

The modules to be extracted and transferred from lalily to openLilyLib are (for example)

1. the template engine,
2. the edition-engraver,
3. the organization of predefined layout and paper definitions,
4. the automatic loading of (library-)files,
5. the automated processing of books and score-collections,
6. ...

1. the template engine
----------------------

Using templates with lilypond where the first part of the lalily-collection of utilities. There is a start of documentation in the lalily repository.
It shall move here together with the separated module. For now you can take a look [here](https://github.com/jpvoigt/lalily/blob/master/examples/lalily-templates.md#lalily-templates).
TBC

2. The [edition-engraver](../../edition-engraver/)
--------------------------------------------

The edition-engraver is the first module I extracted. It prepares an engraver, which can be consisted to any context.
Its main and original purpose is to add tweaks to a score without touching the music input. But it also proved very helpful to add breaks and pageBreaks (or forbid them)
and to add annotations in form of textscripts. 
TBC
