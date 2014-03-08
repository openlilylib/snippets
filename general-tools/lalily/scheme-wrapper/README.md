Scheme-Wrapper and Scheme-Helper
================================

In this folder are a few functions helper functions collected.

parserDefine
------------

To define variables inside a scope that is not toplevel, here is a void-function that does it.
For example

    {
	  mySnippet = \relative c'' { bes4 a c b }
	}

is not allowed. But with the parserDefine command, variables can be defined almost anywhere:

    {
	  \parserDefine mySnippet \relative c'' { bes4 a c b }
	}

add-guile-path
--------------

When you want to define a scheme-module, the path to the files must be known to guile.
The addGuilePath command is a void function that extracts the directory path of the given location argument
and adds the given relative path to %load-path. Now use-modules and load-from-path can access files in the
denoted folder.

In the given example are:

    definitions.ily
    example.ly
    my-scheme/my-init.scm
    my-scheme/my-module.scm

In the example file the my-scheme folder is added to %load-path. Now lilypond can execute

    #(load-from-path "my-init.scm")
	#(use-modules (my-module))

a-list-access
-------------

In a-list-access are commands defined, to work with a-lists and nested a-lists.
With

    \setalist mylist symbol #val

you can set the value 'symbol in the variable 'mylist' to value.
If mylist is an empty list and val contains the value 25, mylist will be set to

    '((symbol . 25))

If you work with nested a-lists, you can use setatree with a path list:

    \setatree myopts my.option.path #25

If myopts is an empty list, this command sets myopts to

    '((my . ((option . ((path . 25)) )) ))

This is quite handy, if you need to prepare options, which are often given as (nested) a-lists.


