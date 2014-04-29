\include "definitions.ily"

\header {
  title = "Git repository information"
}

\markup \vspace #2
\markup \bold "Introduction"
\markup \vspace #1

\markup \justify {
  When working on a project it is essential to tag printouts with at least
  a date and maybe some version information, otherwise the copy is somewhat
  worthless when it comes to proof-reading.
  This is even more true when working under version control, as versioning actually
  can provide you with much more detailed information on the version that is used
  for creating a given PDF or printout.
}
\markup \vspace #0.5

\markup \justify {
  The commands in this snippet provide you with means to print exact versioning
  information somewhere in your score, e.g. in the tagline.
  Of course you should make sure that this doesn't influence the layout of
  your score, otherwise you will run into problems once you want to remove
  the versioning information from the printed result.
}

\markup \vspace #1.5
\markup \bold "Examples"
\markup \vspace #1

\markup"Latest commit (committish): "
\markup \bold \gitCommitish
\markup \vspace #0.5

\markup "Latest commit (shortlog):"
\markup \bold \gitCommit
\markup \vspace #0.5

\markup "Latest commit (full message, doesn't work yet):"
\markup \gitFullCommit 
\markup \vspace #0.5

\markup"Parent commit (committish): "
\markup \bold \gitParentCommitish
\markup \vspace #0.5

\markup "Parent commit (shortlog):"
\markup \bold \gitParentCommit
\markup \vspace #0.5

\markup "Current branch:"
\markup \bold \gitBranch 
\markup \vspace #0.5

\markup "Number of commits on this branch:"
\markup\bold \gitRevisionNumber 
\markup \vspace #0.5

% gitIsClean determines whether the current repository is clean,
% but it only returns a boolean value. So if you want to display
% something based on this information you'll have to create the
% markup for yourself.
#(define-markup-command (isClean layout props) ()
   (if (gitIsClean)
   (interpret-markup layout props
       #{ \markup \bold "has no" #})
   (interpret-markup layout props
       #{ \markup \bold "does have" #})))

\markup \concat { "Repository " \isClean " uncommitted changes" }
