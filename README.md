What's this?
============

A place to store useful pieces of LilyPond code.
As of 2013-09-02, the [Lilypond Snippet Repository](http://lsr.dsi.unimi.it/) accepts snippets for LilyPond version 2.14.2, while the current LilyPond version is 2.17.25.  Until LSR will be upgraded (noone knows when this will happen), I suggest to put all pieces of useful lily code here, just to make sure they won't get lost.


Contributing
============

What's eligible?
----------------

Everything that's useful and isn't totally obvious.  The snippets don't have to be generic or do advanced stuff (although that would be great!) - as a rule of thumb, anything over 20 lines of code is probably worth including.  **You can even contribute work in progress** and update it later - just make sure that the snippet compiles.

How to contribute
-----------------

The easiest way to contribute is via Github's web interface.

1. Create an account on [github](http://github.com/), log into it
2. Go to "snippets" repository's [github page](https://github.com/openlilylib/snippets)
3. To add a new file, click on an icon next to the directory path ([see screenshot](https://raw.github.com/openlilylib/snippets/master/adding-new-file-github-interface.png))
4. To modify a file, click on it and then click _Edit_
5. Write a description of your changes (form at the bottom) and click _Commit .._ button.
6. If you aren't a member of OpenLilyLib organization, you'll be asked to create a pull request. Click on _Send pull request_.

Apart from that you can of course explicitly create a fork and clone into that.
This way you will 'automatically' receive updates of the snippets repository.
And you will be able to edit the repository offline, including the ability to add .png images

Guidelines
----------

A `\version` statement in your snippet is absolutely necessary.

Please use spaces for indenting LilyPond code.  If possible, the easiest way to have minimum problems with indentation is to use Frescobaldi's "Format" tool, if you can.

A snippet can consist of more than one file - enclose them in a directory in such cases.

Please comment your snippet as concisely as possible.
It is good practice to write the snippet as a compilable LilyPond file and create a usage example as part of the documentation.
If you provide an example that is rather complex and needs explanation you can place the snippet in its own subdirectory and provide a README.md file there too.
GitHub will automatically display this [Markdown](http://en.wikipedia.org/wiki/Markdown) formatted file on the webpage.

In case of multi-file snippets, all compilable files should have extension .ly, and all files not meant to be compiled should have extension .ily.

A png file showing the output of the snippet is a welcome addition.  Unfortunately, this cannot be added using github's web interface (we'll add instructions later).

Every time you make some changes in your snippets, please post updates to the repository!  Also, updating snippets so that they will work with latest LilyPond version is welcome.


Snippet categories
==================

* templates
* simple-examples (just explaining things that are already in the documentation)
* custom-engravers (a place for scheme engravers)
* specific-solutions (code solving a very specific user problem)
* standard-snippets (what is now in LSR - code doing something useful)
* stylesheets

Later on, we may divide the snippets into 2 (or more) "quality levels":
- official ones, showing Recommended LilyPond Practice,
- drafts, hacks etc. that were just written by someone and may be useful, but may also not be.


The policy would be to allow anyone to add anything to the "hacks", but adding/changing official ones (or moving a draft to official ones) would require some confirmation from someone else (not necessarily a full review, but at least a quick look).
