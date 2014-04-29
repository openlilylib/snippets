\include "definitions.ily"

\header {
  title = "Git repository information"
}

\markup \concat { "Latest commit (committish): " \gitCommitish }

\markup \concat { "Latest commit (shortlog): " \gitCommit }

\markup \concat { "Latest commit (full message, doesn't work yet): " \gitFullCommit }

\markup \concat { "Current branch: " \gitBranch }

\markup \concat { "Number of commits on this branch: " \gitRevisionNumber }


% gitIsClean determines whether the current repository is clean,
% but it only returns a boolean value. So if you want to display
% something based on this information you'll have to create the
% markup for yourself.
#(define-markup-command (isClean layout props) ()
   (if (gitIsClean)
   (interpret-markup layout props
       #{ \markup "true" #})
   (interpret-markup layout props
       #{ \markup "false" #})))

\markup \concat { "Repository is clean: " \isClean }
