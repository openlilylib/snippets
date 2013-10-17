What's this?
============

A place to store useful pieces of LilyPond code.
As of 2013-10-17, the
[Lilypond Snippet Repository](http://lsr.dsi.unimi.it/)
accepts snippets for LilyPond version 2.14.2,
while the current LilyPond version is 2.17.28.
Until LSR will be upgraded,
I suggest to put pieces of useful lily code here,
just to make sure they won't get lost.

At some point in the future we may develop a nice web frontend for this repository;
we would also like to integrate it with the core LilyPond project similarly to
how stuff from LSR is imported into official documentation.


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


Snippet categories
==================

* [__custom-music-fonts__](custom-music-fonts) -
    alternative fonts for LilyPond, that can be used instead of default Feta.
* [__debugging-layout__](debugging-layout) -
    tools that visualize LilyPond's layout decisions (e.g. directions),
* [__general-tools__](general-tools) -
    stuff for working on and with LilyPond itself.
* [__input-shorthands__](input-shorthands) -
    music functions and other tools that make writing LilyPond code easier,
* [__notation-snippets__](notation-snippets) -
    LilyPond code that produces some particular notation,
* [__simple-examples__](simple-examples) -
    snippets that are just explaining or demonstrating things from the documentation,
* [__specific-solutions__](specific-solutions) -
    hacks that aren't generic, just solve a very specific problem,
* [__stylesheets__](stylesheets) -
    a place for collections of user-designed layout settings ("house styles"),
* [__templates__](templates) -
    examples showing how to structure LilyPond code.

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

Contact
=======

Have trouble contributing?  Let us know!
[info@openlilylib.org](mailto:info@openlilylib.org),
[janek.lilypond@gmail.com](mailto:janek.lilypond@gmail.com).
