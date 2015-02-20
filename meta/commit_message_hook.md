# Well-formed commit message hook

Since OpenLilyLib contains many different libraries, it's desirable
that commit messages start with a name identifying the library. This
way everyone can

However, it is easy to forget to comply to such a policy and to push a
malformed commit message to the upstream repository. Luckily,
[git hooks](http://www.git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
can help to maintain a consistent commit message style.

## Library definition

There's plenty of code in `openLilyLib` but, for the purpose of this
git hook, we define a library as the files contained in a directory
under the `ly` directory of `openLilyLib`. So, for instance,
`ly/_internal`, `ly/scholarly` and `ly/gridly` are libraries.

## Commit message structure

A commit involving a library of `openLilyLib` should be structured in
the following way

```
library: short summary, less than 50 characters

Complete commit. There should be a blank line between the summary and
the commit message. The lines of the commit message should be shorter
than 72 characters.
```

A commit may involve files coming from more than one library. In that
case, the most relevant library is picked as the label of the commit.

## Enforcing commit style with hooks

The python script `commit-msg` will check that the commit message has
the following properties:

  - If one ore more files are in (possibly different) libraries, then
    the commit message should start with the name of one of the
    libraries.
  - The second line should be empty
  - The first line should be shorter than 50 characters
  - All the other lines should be shorter than 72 characters

The script is run automatically by git on every commit, once the commit
message has been written. If the commit message complies with the above
rules, then the commit command can complete successfully. Otherwise all
the errors are reported and the commit fails.

### Installing the hook

Git hooks are placed in the `.git/hooks` directory. For convenience, the
`meta/install-git-hooks.sh` script will create a symlink to the
`commit-msg` hook in `.git/hooks`. So, to enable the hook, from the top
level directory of `openLilyLib` repository

```
meta/install-git-hooks.sh
```

### Handling failures

If the commit fails because the message does not comply with the rules,
all the errors found are printed to the console. The commit message is
saved to the `OLL_COMMIT_MSG` file in the top level directory of the
repository. There you can edit it until the errors are fixed. To repeat
the commit with the new message, the `-F` option of `git commit` can be
used. For instance

```
git commit -a -F OLL_COMMIT_MSG
```

### Skipping the check

Sometimes the rules may be too strict and a commit message may really
need to be in a different format. In this case the check performed by
the hook can be completely skipped, using the `--no-verify` option.

```
git commit -a -F OLL_COMMIT_MSG --no-verify
```

of course the `-F OLL_COMMIT_MSG` part can be skipped if the message has
still to be written.



## FAQ

### Why not use `.git/COMMIT_EDITMSG`?

The file `.git/COMMIT_EDITMSG` contains the last commit message, even if
the commit failed, thus it seems to be the perfect fit for our purpose.

This file also contains all the comment lines inserted by git itself.
The problem is that when used with the `-F` option, git will no longer
see all the lines starting with `#` as comments, thus those lines would
end up in the message, which is undesirable.

### Why is the `OLL_COMMIT_MSG` file placed in the top level directory?

Perhaps it would be more practical to create it in the same directory
where the `git commit` command was issued. Unfortunately it looks like
the hook script does not know which directory is this, since it is
called in the repository's top level directory.
