openLilyLib Snippets - Contributor's Information
------------------------------------------------

This file contains more detailed information how to contribute to
openLilyLib's Snippet Repository.


`\version` statement
--------------------


If your snippet can be compiled with latest stable LilyPond version,
please mark it as such.  If it cannot be compiled with latest stable,
please indicate the latest or earliest version with which it can be compiled.

You may also provide additional information in snippet's `\header`:
- first-known-supported-version
- last-known-supported-version


Documenting snippets
--------------------

To be able to maintain the repository, we need to have some
'metadata', like description, status etc. in the snippet files.

We would also like all snippets to be documented as good as possible.
However, if you don't have time to document your snippet, please
submit it anyway (with "undocumented" status) - we prefer to have
undocumented snippets rather than no snippets.  Of course, we ask
you to add missing documentation later.

It is good practice to write the snippet as a compilable
LilyPond file and create a usage example as part of the
documentation.

If you provide an example that is complex and needs lots of
explanation you can place the snippet in its own subdirectory
and add a [Markdown](http://en.wikipedia.org/wiki/Markdown)-formatted
README.md file to that directory. GitHub will automatically
display this file on the webpage.


Contributing using advanced tools
---------------------------------

Using GitHub's web interface is convenient, but for some tasks like

* uploading png images that show snippets' output,
* modifying more than one file at a time
  (e.g. to add a multi-file snippet),

you have to use more powerful tools.  Please follow
instructions [here](http://help.github.com/articles/set-up-git)
and [here](http://help.github.com/articles/fork-a-repo)
to get an offline copy of the Git repository.
(we will try to add more specific instructions later).


Special code markers
--------------------

We use some keywords in source comments to mark specific issues:

`TODO` - list things that need to be done (not only related
to the code)

`FIXME` - mark problems in the code (similar to `TODO`,
but slightly different - this is for telling what is wrong,
possibly without providing a solution)

`LILYBUG` - note a problem related to LilyPond behaviour.
Add a tracker issue number if there is one.


Miscellaneous tips
------------------

Some contributors experienced problems when trying to paste into the
web editor (they reported that there was no 'paste' in the right-click
context menu).  If this happens to you, try using `ctrl-v` keyboard
shortcut.
