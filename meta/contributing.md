openLilyLib Snippets - Contributor's Information
------------------------------------------------

This file contains more detailed information how to contribute to
openLilyLib's Snippet Repository.


`\version` statement
--------------------

* If the snippet can be compiled with latest stable LilyPond version,
please mark it as such.
* If the snippet doesn't work with latest _development_ version
(i.e. its status is "broken"), please also try to specify the
`last-known-supported-version` field in snippet's `\header`.
* If the snippet requires new functionality that isn't available
in latest stable version, please try to specify the
`first-known-supported-version` field in snippet's `\header`.


Documenting snippets
--------------------

To be able to maintain the repository, we need to have some
'metadata', like description, status etc. in the snippet files.

We would also like all snippets to be documented as good as possible.
However, if you don't have time to document your snippet, please
submit it anyway (with "undocumented" status) - we prefer to have
undocumented snippets rather than no snippets.  Of course, we ask
you to add missing documentation later.

All snippets should have a usage example.  In some cases (for example
snippets from "simple-examples" category), the snippet itself will
provide an example, but in case of "advanced" snippets the example
should be placed in a separate file.  This is because we want the
snippets to be "includeable": if a snippet defines some useful function,
we want to just download an `.ily` file with the definition and `\include` it,
without having to remove usage examples from the included file.
Such multi-file snippets should always be placed in their own subdirectories.

If your snippet is complex and needs lots of explanation, we recommend
writing it in a [Markdown](http://en.wikipedia.org/wiki/Markdown)-
formatted `README.md` file.  GitHub automatically displays the `README.md`
file when one's viewing a directory in GitHub's web interface.


Pull Requests
-------------

If you add or modify a snippet in `openlilylib/snippets` repository,
but you don't have write access to the repository yet, GitHub will
fork the repository for you (i.e. create your own copy of the
repository), and save your change in your fork.  To have your change
added to the main repository, you have to send a _pull request_.
A pull request is like saying _"Hey, I have some stuff to share.
Please look at it and include it in the main repository"_.

If you don't send a pull request, your changes will remain only in
your fork and they most probably won't be noticed (after all, it's
just your own fork - people look at the original repository).

What if you had forgotten to send the pull request initially?
Go to your fork (`github.com/yourname/repositoryname`),
click on _pull requests_ (on the right), then _new pull request_.
You may have to edit what branch will be sent as a pull request -
to do this, click [_Edit_]
(http://raw.github.com/openlilylib/snippets/master/meta/editing-pull-request.png).

The _base fork_ is where you want changes to be applied (so it should
probably be `openlilylib/snippets` reepository and `master` branch).
The _head fork_ is the repository from which the changes should be
taken (usually your fork), and _compare_ is the branch that you want
to be merged.


Contributing using advanced tools
---------------------------------

Using GitHub's web interface is convenient, but for some tasks like

* uploading png images that show snippets' output,
* modifying more than one file at a time
  (e.g. to add a multi-file snippet),

you have to use more powerful tools.  Please follow
instructions [here](http://help.github.com/articles/set-up-git)
(we don't recommend using "GitHub for Windows/Mac" -
in our opinion it's better to use "ordinary" Git tools)
and [here](http://help.github.com/articles/fork-a-repo)
to get an offline copy of the Git repository.
(we will try to add more specific instructions later).  
Note that you can use uploaded png files in your
documenting Markdown files.


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
