
lalily
======

> A lot of needed code is placed in scheme-modules now, so you have to keep the folder structure inside the lalily folder!

The core functions of [lalily](https://github.com/jpvoigt/lalily/) shall be separated and provided as independant modules in this folder.

These are (for example)

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

2. The [edition-engraver](edition-engraver/)
--------------------------------------------

The edition-engraver is the first module I extracted. It prepares an engraver, which can be consisted to any context.
Its main and original purpose is to add tweaks to a score without touching the music input. But it also proved very helpful to add breaks and pageBreaks (or forbid them)
and to add annotations in form of textscripts. 
TBC
