What's this?
============

A place to store useful pieces of LilyPond code.
As of 2013-09-02, the
[Lilypond Snippet Repository](http://lsr.dsi.unimi.it/)
accepts snippets for LilyPond version 2.14.2,
while the current LilyPond version is 2.17.25.
Until LSR will be upgraded (noone knows when this will happen),
I suggest to put all pieces of useful lily code here,
just to make sure they won't get lost.

At some point in the future we may develop a nice web frontend for this repository,
but for now it is just good to know there is a place for useful LilyPond code.


Contributing
============

What's eligible?
----------------

Everything that's useful and isn't totally obvious.
The snippets don't have to be generic or do advanced stuff
(although that would be great!) - as a rule of thumb,
anything over 20 lines of code is probably worth including.
**You can even contribute work in progress** and update it later -
just make sure that the snippet compiles.

How to contribute
-----------------

The easiest way to contribute is using GitHub's web interface.

1. Create an account on [GitHub](http://github.com/), log into it.
2. Go to this repository's
[GitHub page](https://github.com/openlilylib/snippets).
3. To add a new file, [click on an icon]
(https://raw.github.com/openlilylib/snippets/master/meta/adding-new-file-github-interface.png)
next to the directory path.
To modify a file, click its name and then click _Edit_.
5. Write a description of your changes (form at the bottom)
and click _Commit_.
6. Click _Send pull request_.

If you'd like to do more (upload png images showing snippet's
output, modify more than one file at a time, etc.), you have to
use something more powerful than web interface.  Please follow
instructions [here](https://help.github.com/articles/set-up-git)
and [here](https://help.github.com/articles/fork-a-repo)
(we will try to add more specific instructions later).


Guidelines
----------

This repository can only be maintained if the snippets are self-contained
and self-documented to some extent. For this we are using some kind of
'medadata' in the snippet files.
Please use the file `snippet-template.ly` in the root directory of the repo
as the starting point for your contribution.  
In addition to the 'mandatory' keys that are present in the template you
can use the following entries in the `\header`:

- __status__ (e.g. undocumented)
- __first-known-supported-version__
- __last-known-supported-version__ (to help users pick suitable snippets)

A `\version` statement is necessary.  If your snippet can be
compiled with stable LilyPond version, please mark it as such.

If possible, please format your code using Frescobaldi's
_Format_ tool.

Multi-file snippets should be placed in their own directories
(this cannot be done using the web interface).  All compilable
files should have extension `.ly`, and all files not meant
to be compiled should have extension `.ily`.

Please comment your snippet as concisely as possible.
(However, if you don't have time to document it, please submit it and
mark its status as "undocumented". we prefer to have undocumented snippets
rather than no snippets.  Of course, adding documentation later is very
welcome.)
It is good practice to write the snippet as a compilable LilyPond file and create a usage example as part of the documentation.
If you provide an example that is rather complex and needs explanation you can place the snippet in its own subdirectory and provide a README.md file there too.
GitHub will automatically display this [Markdown](http://en.wikipedia.org/wiki/Markdown) formatted file on the webpage.

A png file showing the output of the snippet is a welcome addition.
Unfortunately, this cannot be added using github's web interface.

Every time you make some changes in your snippets, please post
updates to the repository!  Also, updating snippets so that they
will work with latest LilyPond version is welcome.

You can see more guidelines in
[`meta/contributors-guide.md`]
(https://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md),
but you're not required to read them.


Snippet categories
==================

* __custom-engravers__ - scheme engravers, especially ones
    that could be later added to LilyPond itself,
* __input-shorthands__ - music functions and other tools
    that make writing LilyPond code easier,
* __notation-snippets__ - LilyPond code that produces
    some particular notation,
* __simple-examples__ - snippets that are just explaining
    or demonstrating things from the documentation,
* __specific-solutions__ - snippets that aren't generic but just solve
    a very specific problem can be placed here (even "ugly hacks"),
* __stylesheets__ - a place for collections of user-designed
    layout settings ("house styles"),
* __templates__ - examples showing how to structure LilyPond code.

Every category has a `README.md` file inside with more details,
but if you're not sure which category to choose, don't worry!
*It's not that important.*


<!---
Later on, we may divide the snippets into 2 (or more)
"quality levels":
- official ones, showing Recommended LilyPond Practice,
- drafts, hacks etc. that were just written by someone
  and may be useful, but may also not be.

The policy would be to allow anyone to add anything to the "hacks",
but adding/changing official ones (or moving a draft to official ones)
would require some confirmation from someone else (not necessarily
a full review, but at least a quick look).

Update: actually, the status field probably already does this.
-->
