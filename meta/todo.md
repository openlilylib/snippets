* add instructions on how to work with the repo
with something else than github's web interface
* add instructions for "uploading" png files
* add info about branching (when we will have some policy)
* decide on licensing
* add a script that would automatically produce missing
png images (using appropriate lily versions for each snippet -
maybe it could even set up the "dev environment", i.e.
prepare appropriate lilyponds using Janek's script
http://github.com/janek-warchol/cli-tools/blob/master/lilypond/build-lily.sh ?)
* mention some cool stuff that may not be obvious to some people:
    * that you can easily link to files and folders
Find a way to automatically crop png output.
answer: 'mogrify -trim image' should work
Merge other useful lily repositories, for example:
* https://github.com/mwitmer/LyUtil
* https://github.com/jpvoigt/lalily
* orchestrallily
* i think that we should gather together various lilypond
  repositories under the banner of OpenLilyLib.  For example
  * https://github.com/dliessi/ports
