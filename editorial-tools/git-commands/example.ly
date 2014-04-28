\include "definitions.ily"

\header {
  title = "Git repository information"
}

\markup \concat { "Latest commit (committish): " \gitCommitish }

\markup \concat { "Latest commit (shortlog): " \gitCommit }

\markup \concat { "Latest commit (full message, doesn't work yet): " \gitFullCommit }

\markup \concat { "Current branch: " \gitBranch }

\markup \concat { "Number of commits on this branch: " \gitRevisionNumber }