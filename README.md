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

At some point in the future we may develop a nice web frontend for this repository.


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
5. Write a description of your changes (form at the bottom)
and click _Commit_.
6. Click _Send pull request_.

Using more powerful tools for contributing is described [here](http://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md#contributing-using-advanced-tools).


Guidelines
----------

_Note: detailed guidelines are in [`meta/contributors-guide.md`]
(http://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md)
(optional reading)._

* Your snippet must compile (even if it's work-in-progress)
and it must contain a `\version` statement. ([read more]
(http://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md#version-statement))
* Please use [`snippet-template.ly`]
(http://github.com/openlilylib/snippets/blob/master/snippet-template.ly)
as your starting point, filling all header fields with appropriate
information (choose the status of your snippet from the list in
[`meta/status-values.md`]
(http://github.com/openlilylib/snippets/blob/master/meta/status-values.md)).
* Apart from writing a description, we recommend adding
usage example(s) to your snippet. ([read more]
(http://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md#documenting-snippets))
* If possible, please format your code using Frescobaldi's
_Format_ tool.
* When you make changes in your snippets, please contribute
updates to the repository! :-)
* A png file showing the output of the snippet is a welcome addition.
See [advanced contributing tools]
(http://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md#contributing-using-advanced-tools).
* Multi-file snippets should be placed in their own directories
([read more](http://github.com/openlilylib/snippets/blob/master/meta/contributors-guide.md#contributing-using-advanced-tools)).
All compilable files should have extension `.ly`, and all files
not meant to be compiled should have extension `.ily`.


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
