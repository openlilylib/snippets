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
“install” it.

### Installation

“Installing” openLilyLib/snippets consists of two separate steps: obtaining the
repository and making it available to LilyPond.

The current state of the repository can always be downloaded from Github using
the green download button on the repository page or the [direct
link](https://github.com/openlilylib/snippets/archive/master.zip). This is
appropriate for occasional use but we strongly recommend using
[Git](http://git-scm.com/) to clone the whole repository to your computer.
Learning a version control system like Git is one of the most rewarding
investments you may consider doing anyway. (You may read more on this browsing
the [version control tag](http://lilypondblog.org/tag/version-control/) on
*Scores of Beauty*, our blog).

The repository may be saved anywhere on your disk, but we recommend creating a
“root” directory for openLilyLib, e.g. `~/openlilylib` or `C:\openlilylib` and
placing the repository within that (`~/openlilylib/snippets`,
`C.\openlilylib\snippets`).

This “root” directory should then be added to LilyPond's search path, which is
done by using the `-I` or `--include=` command line argument in LilyPond's
invocation (`lilypond -I "~/openlilylib" my-file.ly`). However, editing tools
may provide a convenient interface for this, like
[Frescobaldi](http://frescobaldi) where you can add this path in the LilyPond
page of the Preference dialog.

**Note** (for users who already use openLilyLib): This recommendation is
different from the previous recommendation to add the `snippets` directory
itself to the search path. This change is done to make the snippets repository
consistent with the other openLilyLib packages. Of course this will require
existing documents to be updated!

Most snippets are divided into an `.ily` file with function definitions and a `.ly`
file showing a usage example.  To use the functions provided by the snippet,
simply `\include` the `.ily` file into your score.

### Usage

#### Direct (Standalone) Use

#### Usage As openLilyLib Package



Contributing
============

What's eligible?
----------------

Everything that's useful and isn't totally obvious (rule of thumb:
anything over 20 lines of code is probably worth including).
**You can even contribute work in progress** and update it later.


How to contribute
-----------------

The easiest way to contribute is using GitHub's web interface.

1. Create an account on [GitHub](http://github.com/), log into it.
2. Go to this repository's
[GitHub page](http://github.com/openlilylib/snippets).
3. To add a new file, click on [this button]
(http://raw.github.com/openlilylib/snippets/master/meta/adding-new-file-github-interface.png).
To modify a file, click its name and then click _Edit_.
5. Write what you have changed (form at the bottom)
and click _Commit_.
6. **Don't forget to click** _Send pull request_.
([more info...](meta/contributing.md#pull-requests))

Using more powerful tools for contributing is described
[here](meta/contributing.md#contributing-using-advanced-tools).


Guidelines
----------

_Note: detailed guidelines are in [`meta/contributing.md`]
(meta/contributing.md)(optional reading)._

* Your snippet must compile (even if it's work-in-progress)
and it must contain a `\version` statement.
* Please use a template from [`meta/snippet-templates`]
(meta/snippet-templates) as your starting point:
  - simple snippets that just demonstrate built-in LilyPond features
    should use the one-file template.
  - snippets defining custom commands, functions, engravers etc.
    that can be useful on their own should use the "includable" template.
* If possible, please format your code using Frescobaldi's
_Format_ tool.
* When you make changes in your snippets, please contribute
updates to the repository! :-)


Contact
=======

Have trouble contributing?  Let us know!  
[info@openlilylib.org](mailto:info@openlilylib.org)  
[janek.lilypond@gmail.com](mailto:janek.lilypond@gmail.com)  
