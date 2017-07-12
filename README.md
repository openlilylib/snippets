# openLilyLib/snippets

## What's this?

What is `openLilyLib/snippets`? There are two answers to this: it's a repository of LilyPond snippets,
and it's part of the [https://github.com/openlilylib](openLilyLib) family of extension packages.

#### The Snippets Repository

Originally this repository began as an extended version of the
[Lilypond Snippet Repository](http://lsr.di.unimi.it/LSR/Search), a place to store LilyPond tools -
snippets, templates, extensions. The main differences to the LSR are:

* Some things cannot be placed in LSR (multi-file extensions, special scripts).
* LSR doesn't have tools for collaboration and version control.
* There is not *one* LilyPond engine behind the system, so snippets do not necessarily all
  have to work with the same (sometimes outdated) version.

But maybe the most important difference is that the snippets in this repository are *includable*.
The snippets in the LSR have to be copied over into a user's personal library or documents,
while the snippets in openLilyLib/snippets are *used* by having the library available and referencing
the code directly in LilyPond input files.

<!---
At some point in the future we may develop a nice web frontend for this repository;
we would also like to integrate it with the core LilyPond project similarly to
how stuff from LSR is imported into official documentation.
-->

#### The openLilyLib Package

But the `snippets` repository is also a member of the family of `openLilyLib` packages.
In this family it is sort of an exception as different from the other packages the
snippets may also be used directly, without making use of the central
[oll-core](https://github.com/openlilylib/oll-core) package.

**[EDIT:** *Currently `openLilyLib` is undergoing a fundamental reorganization.  
The below README contents is about the* current *implementation as a
collection of arbitrary snippets.  
The new structure as a collection of targeted libraries can be found inside
the `ly` directory.]*

## Contents and Structure

In general all directories and subdirectories contain snippets or *modules*.
We don't keep a table of contents here (as we are waiting for a documentation
system that will do that properly), so in order to find your way around please
browse the structure

However, the following directories do *not* contain modules:

* `fonts`  
  is a deprecated Python script to install additional notation fonts (probably defunct)
* `fried-library-to-be-sorted`  
  as the name suggests this is a heap of unsorted stuff, enter at your own risk
* `ly`  
  contains a number of “old-new-style” packages (see below)
* `meta`
* `test`




## Using this repository

You can view the files in your browser by clicking on their names, and simply
copy & paste the code into your editor. Each snippet should contain both an
includable `.ily` file (usually `module.ily`) and a compilable `.ly` file
(usually `example.ly`). But the preferred way of using `snippets` is to
“install” it as explained on this [Wiki
page](https://github.com/openlilylib/oll-core/wiki#installation). The direct
link to the ZIP file is
[here](https://github.com/openlilylib/snippets/archive/master.zip).

**For those who don't read manuals** (or who are already using openLilyLib):  
The referenced installation instruction *differs* from earlier recommendations
how to set up `openLilyLib/snippets`: the repository may still be saved anywhere
on your disk, but we recommend creating a “root” directory for openLilyLib, e.g.
`~/openlilylib` or `C:\openlilylib` and placing the repository within that
(`~/openlilylib/snippets`, `C.\openlilylib\snippets`). It is now *this root
directory* (and not the `snippets` subdirectory) that should be added to
LilyPond's include path. This is to make the snippets repostory consistent with
the other openLilyLib packages. Of course this means that the include links for
existing documents have to be updated accordingly.


### Direct (Standalone) Use

The `snippets` repository is an exception in the family of openLilyLib packages,
as it can be used standalone or as a package. We are heading towards the package
approach in general, but for now the standalone use may be the most convenient
and quick application for some users.

Basically each snippet contained in this repository is represented by one
subdirectory, using a nested structure. For example, everything in the
`editorial-tools/line-break-marks` directory constitutes the
`snippets.editorial-tools.line-break-marks` snippet.

Most snippets are maintained in an includable `.ily` file with function
definitions, which should generally be named `module.ily` (e.g.
`editorial-tools/line-break-marks/module.ily`). Assuming the recommended set-up
of having the openLilyLib root directory in LilyPond's include path this can
then be used with:

```lilypond
\include "snippets/editorial-tools/line-break-marks/module.ily"

{
  % use commands defined in the snippet
  R1 \lineBreakMark R1
}
```

Most snippets also have at least one compilable example file demonstrating its
use. This is usually named `example.ly`. Note that (generally) the example files
use the snippet with the package syntax described below but they mostly contain
a commented include statement for standalone use.

### Usage as openLilyLib Package

While it's perfectly possible to directly include snippets as described above we
encourage all users to load them as *modules* as described on the [general
Wiki](https://github.com/openlilylib/oll-core/wiki/Basic-Usage) page, using the
`\loadModule` and `\loadModules` commands.

Note that `snippets` does not provide any package or module options (yet), and
as it doesn't contain top-level modules there is no practical use for the
`\loadPackage \with { modules = ... }` syntax.
