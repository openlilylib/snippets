GridLY - Simple segmented grid for LilyPond
===========================================

This library implements the "segmented grid" approach described in
[this blog post](http://lilypondblog.org/2014/10/segment-grid/) by Urs
Liska, with some ideas about "putting" and "getting" the music from
[this blog post](http://lilypondblog.org/2014/07/trees-music-and-lilypond/)
by Jan-Peter Voigt, both appeared on lilypondblog.org.

GridLY is part of `openLilyLib` and is maintained by
[Matteo Ceccarello](mailto:matteo.ceccarello@gmail.com)

---

The segmented grid approach consists in dividing a multi-part score in
many segments that can be edited independently, possibly my many
people at the same time. From the post of Urs Liska

> A score is naturally organized in a set of “voices” or “parts” – any
> LilyPond user would enter each part in a separate music variable and
> maintain it in its own file. But a score usually is also organized
> over time, usually with rehearsal marks, which form a natural way to
> define the “columns” of our grid.

Having each element of the grid in its own file has several
advantages, as pointed out in the original blog post:

 - each file is small and manageable
 - single elements can be compiled standalone, thus reducing the time
   spent waiting for the LilyPond compiler while entering music
 - using many small files under version control reduces the risk of
   incurring into merge conflicts

Motivation
----------

I found the "segmented grid" approach extremely useful, even for
working on small scores on my own. Having the score split into small
segments sets some natural breakpoints for the work. Moreover you are
less likely to mess up everything because of misalignment errors,
because the grid, together with bar checks, keeps everything in its
place.

However, this approach has some drawbacks for me:

 - you have to preprocess the input files every time you start a new
   project, using some custom scripts
 - you can't easily change the structure of the grid, i.e. add new
   segments in the middle or changing the split point between
   segments.
 - if you want to select a subset of the segments (for instance you
   want to prepare a score with only segments from D to F) you have to
   create another LiliPond file that assembles only the desired subset
   of variables

In an ideal world, you never make mistakes, so you get the right
number of segments and the right separation points between them from
the beginning. In the real world, once the project is well on its way,
you may discover that a segment should really have started at some
other measure. In such a situation changing the grid structure is
really difficult, because you have to manually change all the bar
checks in all the files while changing the duration of some
segments. And you need to get it right, otherwise there will be errors
due to misalignment with error messages that are not useful at all.

This is due to the fact that all the segments are stored in LilyPond
variables, but LilyPond itself does not have any information about
these segments and their structure. The purpose of `gridly` is just
that: to provide some information about the structure, so that
refactoring the grid, slicing it and manipulate it in general can be
done more easily.

The grid
--------

As said earlier, the score is divided into parts and segments. The
grid that originates from such a division is composed by so-called
_cells_. Each cell has several attributes:

 - `music`: this is mandatory, it is the music contained in that
   particular cell.
 - `opening`: when a segment is compiled standalone, it might need
   some commands to be executed before the start of the actual
   segment. For instance you may need to issue a `\partial` command,
   or set the `\time` and so on. The `opening` attribute can be used
   for such things, and is optional.
 - `closing`: the dual of the `opening` attribute. The place to put
   finishing stuff in, like to end slurs and spanners.
 - `lyrics`: the optional lyrics associated to the `music`.

Public interface
----------------

Before describing the public interface of `gridly`, let's see a couple
of things that are used in almost all the functions.

### Segment selectors

Used in functions to get the music out of the grid, segment selectors
are either scheme pairs, integers or the symbol `'all`. The latter selector is
used to select all the segments in the grid, an integer selects a
single segment, whereas the pair specifies a range, with start and end points
included. So `'(3 . 6)` will select all the segments from `3` to `6`, included.

In the public functions description, segment selectors are identified
by `seg-sel`.

### Optional attributes via context modifiers

Some cell attributes of a cell, like `opening`, `closing`, and
`lyrics`, are optional. These attributes can be set using a context
modifiers (for examples take a look at
[example/example.ly](https://github.com/Cecca/gridly/blob/master/example/example.ly)).

The following snippet sets a context modifier with some lyrics and an
opening
```lilypond
\with {
  opening = {
    \time 3/4
  }
  lyrics = \lyricmode {fa la la la!}
}
```

In the public functions description, context modifiers are identified
by `ctx-mod`.

### Public functions

In the following list, arguments surrounded by `< >` are mandatory,
whereas arguments surrounded by `[ ]` are optional.

 - `\gridInit <num-segments> <part-list>` : initializes the grid with
   the given number of centers and the given parts. This is the first
   thing to do. Subsequently, if you try to set/access a cell outside
   of the segment range or not listed in `<part-list>`, you will get
   an error.

 - `\gridSetStructure <seg-sel> [ctx-mod] <music>` :
   this function can be optionally called to set the structure of the
   given segment, for all voices.

 - `\gridPutMusic <part> <seg-sel> [ctx-mod] <music>` :
   this function inserts the given music in the given position of the
   grid.

 - `\gridDisplay` takes no arguments, and prints to the console the
   current state of the grid, with `o` marking the inserted cells and
   `-` marking missing ones.

 - `\gridCheck` : checks that all the parts within a segment have the
   same duration. If the structure of that segment has been specified,
   then the duration specified there is used as a reference, otherwise
   the duration of the various parts are compared among themselves.

 - `\gridGetMusic <part> <seg-sel>` : returns the music associated
   with the given part matching the given segment selector. The
   segments are returned as a single music expression, thus can be
   readily included in voices and staves.

 - `\gridGetLyrics <part> <seg-sel>` : the same as `\gridGetMusic`,
   but returns the lyrics of all the segments, concatenated. Throws an
   error some segment is missing lyrics.

 - `\gridGetSructure <seg-sel>` : same as `\gridGetMusic`, but returns
   the contents of the structure of the given segments. In this way
   the structure part can be used to store things like rehearsal
   marks, tempo changes and so on.

 - `\gridTest <part> <seg-sel>` : put in the same file as the call to
   `\gridPutMusic` with the same parameters, compiles the cell defined
   in the file as a standalone score.

Usage
-----

For an example of usage on a single file, see
[example/example.ly](https://github.com/Cecca/gridly/blob/master/example/example.ly).

Instead, for an example of a multi-file score, take a look at
[example/multi-file](https://github.com/Cecca/gridly/tree/master/example/multi-file).
