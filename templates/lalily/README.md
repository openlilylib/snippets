
creating templates for lilypond
===============================

lalily templates
----------------

With lalily-templates music is organized in `folders` - one might call it namespaces.
In lalily it is called `music-folder` because music is stored in a file-system-tree like manner.
The music-folder is addressed by a path, which is actually a list.
The templates are `music-function`s with a fixed signature: `(piece options)(list? list?)`
The `piece` argument is the current music-folder (or namespace) and the
`options` argument is an association-list with all needed arguments.
Also the templates are stored in such a foldertree,
so they are also called by their respective path.

To engrave music, the template function is called with a current music-folder.
For example:

    \callTemplate lalily.vocal.group my.choral.music #'() % alternatively you can `\LY_NOOP` for empty options

This calls a template with a path `#'(lalily vocal group)`
(thanks to David K.s parser improvements, it can be entered in dot-notation since lilypond 2.17.?)
The music-folder is set to `#'(my choral music)` and options are empty - SATB four stave is default for the template.
When the template-function returns, the music-folder is reset to its previous value.
On the console (at least in *nix-like systems) it is equivalent to `cd -`.
Inside the template function, the music is accessed using `\getMusic sop.melody`,
to get the melody for the soprano voice.
In affect music is taken from a music-folder `my.choral.music.sop.melody`.
And if there is a template to create a staff with a /relative/ template name `staff`,
it can be called with `\callTemplate staff $voice $voice-options`,
where voice and voice-options may be variables inside a loop,
which name the /relative/ path and the related options.
That might be `#'(ten)` for the path and `#'((clef . "G_8")(name . "Tenor"))` for the options.
And the loop might result from a given list in the wrapping template.

This leads to several possibilities. For example if you have a piece for two choirs,
you might call the template twice in a wrapping template with namespaces `A` and `B`.
And if you want to combine multiple scores in one book, they are separated by namespace or stored in different music-folders.

In the lalily framework, a default or standard template path with options can be bound to a music-folder.
So if you `\include "lalily.ly"`, templates in defined places are automatically loaded.
Now you can start with some shortcut functions:

```
\version "2.18.0"
% use -Ipath/to/lalily to make lilypond find lalily.ly
\include "lalily.ly"

% set music-folder to my.choral.music
% bind template lalily.vocal.group to this folder
% use default options
\setDefaultTemplate my.choral.music lalily.vocal.group #'()
% bind title "SATB" to this folder
\setTitle "SATB"
% bind composer with name "lalily" and life "(*2011)" to this folder
% it may be a string, a pair of name and life or a symbol, which refers to name and life data stored in lalilys registry
\setComposer #'("lalily" . "(*2011)")

% prepare meta part of music with time and key signature, this is often called "global"
\putMusic meta { \time 4/4 s1 \bar "|." }

% insert melody
\putMusic sop.music \relative c'' { c4 }
\putMusic alt.music \relative c' { c4 }
\putMusic ten.music \relative c' { c4 }
\putMusic bas.music \relative c { c4 }
% insert text
\putMusic sop.lyrics \lyricmode { sop }
\putMusic alt.lyrics \lyricmode { alt }
\putMusic ten.lyrics \lyricmode { ten }
\putMusic bas.lyrics \lyricmode { bas }
% engrave score if parser output name matches current location, that is, if this file is not included, but compiled directly
\lalilyTest
```

In this example are a few things, which lalily also does: Beside the template name and its associated options,
other header information like title and composer can be bound to a music-folder.
With `\lalilyTest` the score is engraved, but only if the parser output name matches the filename found in the location argument of the lalilyTest function.
That is the case, if the file is compiled directly and not included by another file.
This way, you can enter a choral score in one file, include it in another file and engrave it inside a book together with other pieces.

TBC
