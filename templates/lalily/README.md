`TBD` copy and adapt code from [lalily](https://github.com/jpvoigt/lalily)

creating templates for lilypond
===============================

[skip to lalily-templates](#lalily-templates)

introduction
------------

The first scores I engraved with lilypond where coded all inline.
All notes where surrounded by the needed `\new Staff` or `Voice` constructs and also `\score`.
But when it came to the second SATB choral score, I started to use variables and first copied,
then included the needed structure.
But this also didn't met my needs fully, when I had to copy and then edit the needed wrapper for most scores.
So I started to use scheme and create `music-function`s, which let me enter the needed changes to the wrapper with parameters to a function.

For example, one might create a function, which conditionally creates a SATB score in two or four staves:
(jump to [lalily-templates](#lalily-templates))

```
\version "2.18.0"

% create a function, which creates conditionally 2 or 4 staves
SATB =
#(define-music-function (parser location type)(symbol?)
   (cond
    ((eq? type 'two)
     #{
       % create a ChoirStaff with two staves and lyrics above and below
       \new ChoirStaff <<
         % create lyrics context to be filled later
         \new Lyrics = "soptxt" { \skip 4 }
         \new Staff \with {
           instrumentName = "S/A"
         } <<
           \new Voice = "sop" { \voiceOne \sop }
           \new Voice = "alt" { \voiceTwo \alt }
         >>
         % insert soprano lyrics into formerly created lyrics context above the stave
         \context Lyrics = "soptxt" \lyricsto "sop" \soptxt
         % create alto lyrics
         \new Lyrics = "alttxt" \lyricsto "alt" \alttxt
         
         % the same for tenor and bass
         \new Lyrics = "tentxt" { \skip 4 }
         \new Staff \with {
           instrumentName = "T/B"
         } <<
           { \clef bass }
           \new Voice = "ten" { \voiceOne \ten }
           \new Voice = "bas" { \voiceTwo \bas }
         >>
         \context Lyrics = "tentxt" \lyricsto "ten" \soptxt
         \new Lyrics = "bastxt" \lyricsto "bas" \alttxt
       >>
     #})
    (else
     #{
       % if the argument is not 'two, create ChoirStaff with four staves
       \new ChoirStaff <<
         \new Staff \with {
           instrumentName = "Sopran"
         } \new Voice = "sop" { \sop }
         \new Lyrics \lyricsto "sop" \soptxt
         
         \new Staff \with {
           instrumentName = "Alt"
         } \new Voice = "alt" { \alt }
         \new Lyrics \lyricsto "alt" \alttxt
         
         \new Staff \with {
           instrumentName = "Tenor"
         } \new Voice = "ten" { \clef "G_8" \ten }
         \new Lyrics \lyricsto "ten" \tentxt
         
         \new Staff \with {
           instrumentName = "Bass"
         } \new Voice = "bas" { \clef "bass" \bas }
         \new Lyrics \lyricsto "bas" \bastxt
       >>
     #})))

% now create the variables

sop = \relative c'' { c4 }
alt = \relative c' { c4 }
ten = \relative c' { c4 }
bas = \relative c { c4 }
soptxt = \lyricmode { la }
alttxt = \lyricmode { la }
tentxt = \lyricmode { la }
bastxt = \lyricmode { la }

% engrave two stave system
\score {
  \SATB two
  \layout { }
}
% engrave four stave system
\score {
  \SATB four
  \layout { }
}
```

Still there are copied parts inside this function.
It's no magic to create a function, which creates a vocal stave with lyrics,
but variable naming and variable placing gets a little bit tricky.
The musical and the layout information are still mixed.
And when one wants to combine multiple scores this way, it gets even more tricky.

lalily templates
----------------

I want to be able to code the music once and then include and engrave it (almost) anywhere.
When you develop in object oriented computer languages like for example Java or C++,
you will know the concept of namespaces and of inheritance.
So I asked myself: What would it mean to organize the music like that?
The result is, that the functions in my templating system are called within an active namespace set.

In lalily it is called `music-folder` because music is stored in a file-system-tree like manner.
(I might change the naming in the future to namespace)
The music-folder is addressed by a path, which is actually a list.
The templates are `music-function`s with a fixed signature: `(piece options)(list? list?)`
The `piece` argument is the current music-folder (or namespace) and the
`options` argument is an association-list with all needed arguments.
Also the templates are stored in such a folder-, directory- or namespace-tree,
so they can be called by their respective path.
To engrave music, the template function is called with a current namespace.
For example:

    \callTemplate choral.group my.choral.music #'() % alternatively you can `\LY_NOOP` for empty options

This calls a template with a path `#'(lalily vocal group)`
(thanks to David K.s parser improvements, it can be entered in dot-notation since lilypond 2.17.?)
The namespace is set to `#'(my choral music)` and options are empty --- SATB four stave is default for the template.
When the template-function returns, the current namespace is reset to its previous value.
Inside the template function, the music is accessed using `\getMusic sop.melody`,
to get the melody for the soprano voice.
In affect music is taken from a music-folder `my.choral.music.sop.melody`.
And if there is a template to create a staff with lyrics,
it can be called with `\callTemplate staff $voice $voice-options`,
where voice and voice-options may be variables inside a loop,
which name the /relative/ namespace and the related options.
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
