LilyPond Alternative Font Installation Script
=============================================

`install-lily-fonts` is a Python script that can be used to keep your collection
of alternative notation fonts for GNU LilyPond up to date. As of LilyPond 2.19.12
commands to switch notation fonts are built in to LilyPond, and versions from 2.18
onwards can rather easily be patched to do so too.
More information on the subject, as well as examples, instructions and downloads
can be obtained from http://fonts.openlilylib.org.

With this addition it has become quite straightforward to change LilyPond's
global appearance to a number of styles, from classical engraving to more modern
styles, several handwritten fonts are available as well as fonts modeled from
well-known publishing houses. However, `openLilyLib` provides tools to make this
even simpler, one being this script to manage the font collection, the other being 
the [StylesheetsTODO:LINK](TODO:Link) library with its commands to easily access#
the fonts.

This script helps you with obtaining the whole collection of free notation fonts and
keeping it up to date. At the same time it makes it easy to "install" the fonts to
any number of LilyPond installations that you might have (installations from binary
downloads, from your operating system's package manager or even self-compiled
custom builds).

You can run `python install-lily-fonts --help` at any time to get information on the
available command line options, which is up to date even if this README should not
have been updated.

## Concept

For the time being LilyPond expects its notation fonts in a specific folder
*within* its installation directory. In order to alleviate some issues this
causes with multiple LilyPond installations and/or updates it is advisable to
store the font files in a custom location and only create links to them inside
the LilyPond installation.

This is what `install-lily-fonts` does: it maintains a local font repository with
a catalog of installed fonts and versions. It can detect additions and updates on
the server, downloads and extracts the archives with the font files and manages
the links in the LilyPond installation. So the main information you have to give
the program are the paths to your font repository and to the LilyPond installation
you want to manage.

## Usage

The basic invocation of `install-lily-fonts` is:

```
python install-lily-fonts [options]
```

### Main Configuration

The main options to be noted are:

- `-d --font-directory`  
Pass this option a path to your local font repository, which can be anywhere on
your hard disk. If the option is not present the script considers the current
working directory as the font directory.  
If the resulting path doesn't point to an existing directory you are prompted to choose
between creating the given directory or aborting the program. If the directory exists
but no font catalog file is present the script assumes that you want to create one in
a fresh installation.
- `-t --targets`  
This option is used to determine the target LilyPond installations that should be updated.
It accepts one or more paths to existing LilyPond installations or to files listing
such paths. The path should either point to the root directory of an installation or
to the lilypond executable in its `usr/bin` directory. Both installations from binary
distributions or builds from source are accepted.  
If this fails for any of the given versions the program aborts.

All path arguments can be given as relative or absolute paths. If relative paths
are used they are added to the current working directory before being used.

A targets file lists one LilyPond path per line, empty lines or lines starting with `#`
are silently ignored. The use of targets files is recommended if you have multiple
LilyPond installations that you want to keep up-to-date in parallel. For your convenience
there is an entry `target*` in the script directory's `.gitignore` directory, so you
can add targets files in that practical location without affecting the openLilyLib
Git repository.

### Controlling the Behaviour

Some options can be set to control the behaviour of the program:

- `-b --batch`  
Run the program in batch mode, without any user interaction. Basically this means
that any error condition will immediately abort the script with an error
- `-l --local`  
Don't try to connect to the server to compare the local with the remote font catalog.
Process the given LilyPond installation from the local catalog instead.  
This option can be useful when wanting to update several LilyPond installations
in one go.
- `-f --force`  
Don't try to determine the relation between the local and the remote font catalog
and consider everything local outdated by default.
