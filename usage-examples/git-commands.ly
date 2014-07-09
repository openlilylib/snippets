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

\markup "Latest commit (full message):"
\markup \vspace #0.25
\markup \small \typewriter \gitFullCommit
\markup \vspace #0.25
\markup \italic "(the formatting with typewriter font has been done manually)"
\markup \vspace #0.5

\markup "Latest commit (date/time):"
\markup \bold \gitDateTime
\markup \vspace #0.5

\markup "Latest commit (author):"
\markup \bold \gitAuthor
\markup \vspace #0.5

\markup "Latest commit (email):"
\markup \bold \gitEmail
\markup \vspace #0.5

\markup"Parent commit (committish): "
\markup \bold \gitParentCommittish
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
\markup \vspace #0.25
\markup \italic \justify {
  (Please note that the 'gitIsClean' function only returns a
  boolean value. If you want to display information such as the
  above you will have to create the markup yourself.)
}

%\markup \getgitresult "git status"

\pageBreak

\markup \bold "Printing the full Git diff"
\markup \vspace #0.5
\markup \justify {
  In some cases it may be interesting to print out a full diff
  against the latest commit. Usually one compiles scores
  \italic after modifying them and \italic before committing them.
  So if you really need to have a detailed documentation in the
  printout you can supply the full diff as an additional resource.
  Of course it will make sense to provide it on a separat (last)
  page or even in a separate score or bookpart in order not to
  disturb the score layout. Please note that you're responsible
  yourself for any formatting.
}

\markup \vspace #1
\markup
  \override #'(baseline-skip . 2)
  \tiny \typewriter \gitDiff