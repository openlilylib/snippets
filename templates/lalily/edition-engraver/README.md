lalily edition-engraver
=======================

What is this?
-------------

The edition-engraver is a tool to separate content and display.
It provides a command, which lets you enter

```
\editionMod fullscore 23 1/4 altatrinita.alt.music.Staff.A \shape #'((0 . 0)(0 . 1)(0 . 1)(0 . 0))
```

to modify a slurs shape, which starts in measure 23 on the second fourth
for an edition-engraver tagged with `altatrinita.alt.music`.

Howto use it?
-------------

> You have to `\include "edition-engraver.ily"` from this folder, to make the following work.

To make use of an edition-engraver, you have to consist the target context with a "named" edition-engraver. For example:

```
\new Staff \with {
  % add edition engraver with id-path #'(my test) to this Staff
  \consists \editionEngraver my.test
} <<
  \new Voice \with {
     % add edition engraver to this voice and inherit id-path from parent context: #'(my test) from parent Staff
     \consists \editionEngraver ##f
  } \relative c'' { c4 bes a( g) f e d c }
 >>
```

This will create a Staff context consiting an edition-engraver with a name (or path) of `#'(my test)`. Inside this Staff a Voice
is created consisting an edition-engraver, which inherits its naming path `#'(my test)` from the parental Staff.
Now you can add an edition tag `"fullscore"` to the list of "active" editions:

```
\addEdition fullscore
```

> You can add or remove edition-tags almost anytime, before the engraving is started.

To add tweaks to the Staff or the Voice, you enter them with the `\editionMod` command:

```
\editionMod fullscore 2 1/4 my.test.Staff.A \once \override NoteHead #'color = #red
```

this will color all notes red, occuring in this Staff in measure 2 on the second fourth. The Staff is addressed using the former defined
tag-path `#'(my test)` (or `my.test`) plus the context name `"Staff"` and an alphabetic counter starting with `A`: `my.test.Staff.A` = `#'(my test Staff A)`
With the edition-engraver consisted to this Staff, this entry is equivalent to an override `\once \override Staff.NoteHead #'color = #red`
in the corresponding music-source, but it can be entered somewhere else before the music is actually engraved.

Now the Voice is consisted with an edition-engraver, which inherits the tag-path from the Staff. In this case it leads to and would be
addressed by: `my.test.Voice.A`. With the context-name `"Voice"` in its path, the edition-engraver(s) can distinguish between different
contexts with the same tag-path. A Staff can contain a lot of Voices. If they all inherit there tag-path, we have to have some kind of
attributes, which discrimnates all voices from each other. That is the reason, the counter is added. The counter is counting with characters,
so that we can enter the full path with "dot-notation" - `\editionMod fullscore 2 1/4 my.test.Staff.1` is not parsable.

> There may be a better solution to count contexts.

----

TBC

