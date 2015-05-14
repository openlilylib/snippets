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

Table of contents
-----------------

 - [Introduction](#introduction)
 - [Motivation](#motivation)
 - [The grid](#the-grid)
 - [Public interface](#public-interface)
   - [Segment selectors](#segment-selectors)
   - [Optional attributes](#optional-attributes-via-context-modifiers)
   - [Public functions](#public-functions)
 - [Usage](#usage)
 - [Migration guide](#migration-guide)
   - [From `0.5.*` to `0.6.*`](#from-05-to-06)

Introduction
------------

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
 - `lyrics`: the optional lyrics associated to the `music`. This
   attribute is optional.

Public interface
----------------

Before describing the public interface of `gridly`, let's see a couple
of things that are used in almost all the functions.

#### Segment selectors

Used by the function `gridSetRange` to get the music out of the grid,
segment selectors are either scheme pairs, integers or the symbol
`'all`. The latter selector is used to select all the segments in the
grid, an integer selects a single segment, whereas the pair specifies a
range, with start and end points included. So `'(3 . 6)` will select all
the segments from `3` to `6`, included.

In the public functions description, segment selectors are identified
by `seg-sel`.

### Public functions

All the public music functions defined by GridLY are prefixed with
`grid`.

 - `\gridInit <num-segments> <part-list>` : initializes the grid with
   the given number of centers and the given parts. This is the first
   thing to do. Subsequently, if you try to set/access a cell outside
   of the segment range or not listed in `<part-list>`, you will get
   an error.

 - `\gridSetSegmentTemplate <segment-id> <context-or-music>` : this
   function can be optionally called to set the defaults of the given
   segment, for all parts. Here, the `<segment-id>` is a single
   integer.

 - `\gridPutMusic <part> <segment-id> <context-or-music>` :
   this function inserts the given music in the given position of the
   grid. Here, the `<segment-id>` is a single integer.

 - `\gridDisplay` takes no arguments, and prints to the console the
   current state of the grid, with `o` marking the inserted cells and
   `-` marking missing ones.

 - `\gridCheck` : checks that all the parts within a segment have the
   same duration, for all the segments. If the template of that segment
   has been specified, then its duration is used as a reference,
   otherwise the duration of the various parts are compared among
   themselves. The use of this function is entirely optional. It can be
   used any time you wish to check the grid contents, even multiple
   times.

 - `\gridSetRange <seg-sel>`: sets the range of cells that should be
   retrieved by `gridGetMusic` and `gridGetLyrics`. If this function
   is not called, then the segment ragne defaults to `'all`.

 - `\gridGetMusic <part>` : returns the music associated with the
   given part matching segment selector specified with
   `\gridSetRange`. The segments are returned as a single music
   expression, thus can be readily included in voices and staves.

 - `\gridGetLyrics <part>` : the same as `\gridGetMusic`, but returns
   the lyrics of all the segments specified with `\gridSetRange`,
   concatenated. Throws an error some segment is missing lyrics.

 - `\gridCompileCell <part> <segment-id>` : compiles the given cell in
   a standalone score. Here, the `<segment-id>` is a single integer.

Usage
-----

For an example of usage on a single file, see
[usage-examples/example.ly](usage-examples/example.ly).

Instead, for an example of a multi-file score, take a look at
[usage-examples/multi-file](usage-examples/multi-file).

Migration guide
---------------

Since this software is still in early development, sometimes the
public interface of functions can change to be more expressive and
clear. This sections collects some tips to migrate from one version of
GridLY to another.

### From `0.5.*` to `0.6.*`

#### TL;DR;

In GridLY `0.6.0` the public interface of two functions changed. To
get rid of compilation errors, run the following on the command line.

```bash
sed -s -i -e 's/gridPutMusic /gridPutMusicDepr /g' *.ly
sed -s -i -e 's/gridSetSegmentTemplate /gridSetSegmentTemplateDepr /g' *.ly
```

Then, use the deprecation warnings to upgrade from the old interface to
the new one (see [Migration examples](#migration-examples))

#### Long version

In version `0.6.0` the public interface of functions `gridPutMusic`
and `gridSetSegmentTemplate` have changed. Basically, this change
moves the `music` argument inside the context modifier used to pass
optional arguments, making it clearer to the user which music is part
of which cell. If there are no optional arguments, then both functions
can simply be called with only the music in place of the context
modifier.

Unfortunately, these changes make invocations performed with the old
interface invalid. For instance

```lilypond
\gridPutMusic "soprano" 1
\with {
  lyrics = \lyricmode { Fa la }
}
\relative c' {
  e2 f |
}
```

is no longer valid. Trying to compile this snippet with GridLY `0.6.0`
or above results in an error like

```
example.ly:118:1: No music defined for soprano:1

\gridPutMusic "soprano" 1
fatal error: The `music' argument is mandatory
```

The correct invocation of the function is now

```lilypond
\gridPutMusic "soprano" 1
\with {
  music = \relative c' {
    e2 f |
  }
  lyrics = \lyricmode { Fa la }
}
```

In order to ease the migration, GridLY provides the functions
`gridPutMusicDepr` and `gridSetSegmentTemplateDepr` that use the same
interface of the old `gridPutMusic` and `gridSetSegmentTemplate`,
respectively. These functions delegate the call to their updated
counterparts, issuing a deprecation warning (the suffix `Depr` stands
for _deprecated_).

So, the quick and easy way to get rid of all the errors and still have
all the code in your project to compile is to replace all the occurrences
of `gridPutMusic` with `gridPutMusicDepr` and of
`gridSetSegmentTemplate` with `gridSetSegmentTemplateDepr`. This can
be done by using search and replace in your editor or by using the
command line

```bash
sed -s -i -e 's/gridPutMusic /gridPutMusicDepr /g' *.ly
sed -s -i -e 's/gridSetSegmentTemplate /gridSetSegmentTemplateDepr /g' *.ly
```

in the directory containing the LilyPond files you want to upgrade.

> **WARNING**: Before doing this, I *strongly* suggest to put your
> files under revision control, if you don't already have done this
> (see
> [here](http://lilypondblog.org/2013/08/version-control-text-quality-and-creativity/),
> [here](http://lilypondblog.org/2013/09/write-lilypond-code-for-version-control/),
> and
> [here](http://lilypondblog.org/2014/01/why-use-version-control-for-engraving-scores/)
> for some more discussion on git and LilyPond)

Then, you can use the deprecation warnings as a guidance to update
each call to the new interface. Just remember that, besides pulling
the music argument inside the context modifier, you have to remove the
`Depr` suffix from the name of each function call you update. Once
there are no more deprecation warnings you will know that all the
calls have been upgraded.

#### Migration examples

 1. `gridPutMusic`, no context
    - before

      ```lilypond
      \gridPutMusic "part" 1
      {
        %% The music
      }
      ```
    - after

      ```lilypond
      \gridPutMusic "part" 1
      {
        %% The music
      }
      ```

 2. `gridPutMusic`, with context
    - before

      ```lilypond
      \gridPutMusic "part" 1
      \with {
         %% Optional arguments
      }
      {
        %% The music
      }
      ```
    - after

      ```lilypond
      \gridPutMusic "part" 1
      \with {
         %% Optional arguments
         music = {
           %% The music
         }
      }
      ```

 3. `gridSetSegmentTemplate`, no context
    - before

      ```lilypond
      \gridSetSegmentTemplate 1
      {
        %% The music
      }
      ```
    - after

      ```lilypond
      \gridSetSegmentTemplate 1
      {
        %% The music
      }
      ```

 4. `gridSetSegmentTemplate`, with context
    - before

      ```lilypond
      \gridSetSegmentTemplate 1
      \with {
         %% Optional arguments
      }
      {
        %% The music
      }
      ```
    - after

      ```lilypond
      \gridSetSegmentTemplate 1
      \with {
         %% Optional arguments
         music = {
           %% The music
         }
      }
      ```
