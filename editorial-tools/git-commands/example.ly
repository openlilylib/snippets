\include "definitions.ily"

\header {
  title = "Git repository information"
}

\markup \concat { "Latest commit (committish): " \bold \gitCommitish }

\markup \concat { "Latest commit (shortlog): " \bold \gitCommit }

\markup \concat { "Latest commit (full message, doesn't work yet): " \gitFullCommit }

\markup \concat { "Current branch: " \bold \gitBranch }

\markup \concat { "Number of commits on this branch: " \bold \gitRevisionNumber }


% gitIsClean determines whether the current repository is clean,
% but it only returns a boolean value. So if you want to display
% something based on this information you'll have to create the
% markup for yourself.
#(define-markup-command (isClean layout props) ()
   (if (gitIsClean)
   (interpret-markup layout props
       #{ \markup \bold "no " #})
   (interpret-markup layout props
       #{ \markup \bold "" #})))

\markup \concat { "Repository has " \isClean "uncommitted changes" }
