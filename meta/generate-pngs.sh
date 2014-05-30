#!/bin/bash

# this will generate pngs for all snippets that were changed
# since last checking.

# TODO:
# make updating existing pngs and creating pngs
# for new snippets completely separate steps.

die() { echo "$@"; exit 1; }

while getopts "m" opts; do
    case $opts in
    m)
        only_missing="yes";;
    esac
done

IFS=$(echo -en "\n\b")
working_dir=$(pwd)

saved=$(cat meta/last-checked-commit 2>/dev/null)
git rev-parse $saved &>/dev/null
if [[ -z "$saved" || $? != 0 ]]; then
    # hard-coded repo's initial commi
    last_commit="62f2474f0fd5d906d7c5"
else
    last_commit=$saved
fi

# which snippets we'll be compiling?
echo "Checking against commit $last_commit..."
changed_snippets=$(git diff --name-only $last_commit | grep "\.ly")
if [ "$only_missing" == "yes" ]; then
    snippets_to_compile=$(for f in $changed_snippets; do
        if [ ! -f $(echo $f | sed 's/\.ly/\.png/') ]; then echo $f; fi
    done )
else
    snippets_to_compile=$changed_snippets
fi

if [ -n "$snippets_to_compile" ]; then
    echo "The following snippets will be compiled:"
    for f in $snippets_to_compile; do echo "  $f"; done
    sleep 5
    echo ""
else
    echo "Nothing to do."
    exit
fi

# diagnose versions:
versions_needed=$(for f in $snippets_to_compile; do
    echo $(grep '\\version "' $f | \
    sed 's/^.[^"]*"//' | sed 's/".*$//' | uniq )
done | sort | uniq )

versions_missing=$(for version in $versions_needed; do
    test -d $LILYPOND_BUILD_DIR/release/$version-1/ || echo $version
done )

if [ -n "$versions_missing" ]; then
    echo "The following LilyPond versions have to be compiled:"
    echo $versions_missing
    echo ""
fi

# download lilypond sources if necessary:
if [[ -n "$versions_missing" && -z "$LILYPOND_GIT" ]]; then
    echo "No LilyPond build system detected, downloading..."
    sleep 2; echo ""
    cd $working_dir/../
    wget http://raw.github.com/janek-warchol/cli-tools/master/lilypond/grab-lily-sources.sh || \
    die "Failed to download LilyPond downloading script."
    ./grab-lily-sources.sh
    cd $working_dir
fi

# download building script if necessary:
if [[ -n "$versions_missing" && ! -f ../build-lily.sh ]]; then
    cd $working_dir/../
    wget http://raw.github.com/janek-warchol/cli-tools/master/lilypond/build-lily.sh || \
    die "Failed to download LilyPond building script."
    cd $working_dir
    chmod +x ../build-lily.sh
fi

# compile lilyponds
for version in $versions_missing; do
    ../build-lily.sh -c release/$version-1 || \
    die "Failed to compile LilyPond $version."
done

# produce pngs
for f in $snippets_to_compile; do
    echo "Processing $f ..."
    version=$(grep '\\version' $f | sed 's/^.[^"]*"//' | sed 's/".*$//')
    cd $(dirname $f)
    # this assumes LilyPond builds configured as in my building script
    $LILYPOND_BUILD_DIR/release/$version-1/out/bin/lilypond \
    --png -dinclude-settings=$working_dir/meta/no-tagline.ily \
    $(basename $f) &>/dev/null
    mogrify -trim $(basename $f .ly).png
    cd $working_dir
done
echo "Done."; echo ""

git rev-parse HEAD > meta/last-checked-commit
for f in $(find | grep "\.png"); do
    git add $f
done
git add meta/last-checked-commit

git commit -m \
    "Add pngs to snippets:
    $(echo " "; for f in $snippets_to_compile; do echo $f; done)" || \
    die "Failed to commit changes"
