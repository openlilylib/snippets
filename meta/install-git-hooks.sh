#!/bin/sh

## Creates a symbolic link for `meta/commit-msg` under the
## `.git/hooks` directory.

if [ -e .git/hooks/commit-msg ]
then
    echo "Skipping commit-msg hook, already installed"
else
    ## See http://stackoverflow.com/questions/4592838/symbolic-link-to-a-hook-in-git
    ln -s ../../meta/commit-msg .git/hooks/commit-msg
    echo "Done! commit-msg hook installed"
fi
