\version "2.16.2"

%\include "oll-base.ily"

\header {
  oll-title = "Git repository information"
  oll-author = "Urs Liska, Lars Haulin"
  oll-source = "http://lilypondblog.org/2014/01/why-use-version-control-for-engraving-scores/#comment-34076"
  oll-short-description = \markup {
    Retrieve information about the Git status of a score.
  }
  oll-description = \markup \justify {
    When working on a score it is often necessary to mark the state of
    development on a given intermediate state (printout or shared PDF).
    Instead of simply adding a date it can be very useful to add information
    about the Git revision if the document is in a version control repository.

    This module provides markup commands that retrieve information about the
    current Git repository. It is not only possible to get information about
    the current commit but also whether the repository is in a clean state
    (i.e. whether the score represents the state of a commit or contains
    modifications in the working tree).

    The generic command gitCommand can be used to issue arbitrary Git
    commands. For a list of defined commands see the usage example.
  }
  oll-usage = \markup \justify {
    The generic function gitCommand can be used to invoke a Git
    command and print its result. The command expects a string with the Git
    command without the \typewriter git keyword, e.g. \typewriter
    { rev-parse --short HEAD }. Please note that only the first line of that
    command will be considered. The result is returned as a markup
    column{}. Please be very careful with commands that might
    actually \italic modify the repository because no checks will be done
    whatsoever. You're on your own risk here!

    Usually you will use one of the predefined commands listed below.

  }
  oll-category = "project-management"
  % add comma-separated tags to make searching more effective:
  oll-tags = "versioning,project-history,archiving"
  % is this snippet ready?  See meta/status-values.md
  oll-status = "unfinished, buggy"

  oll-todo = \markup {

    - Gracefully handle missing Git installation

    - Gracefully handle the file not being in a repository

    - Complete a set of useful commands

    - Make it work on other operating systems.

    More ideas:

    - make commands work on specific commits
    (take a committish as an optional argument)

    - Autocommit on LilyPond run???

  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%


#(use-modules (ice-9 popen))
#(use-modules (ice-9 rdelim))

% Read a command, execute it as a shell command and return
% the command's result as a string
% NOTE: This function only reads the first line of the command
% and returns the whole result as one single string
% (in case of multiline results the caller has to split that)
#(define (strsystem_internal cmd)
   (let* ((port (open-input-pipe cmd))
          (str (read-delimited "" port)))
     (close-pipe port)
     (if (eof-object? str) "" str)))

% execute a Git command and return its result as a \markup \column
% with one or more lines in it. If the result is empty an error
% will be raised about unexpected #<eof>
% Pass the function a git command without "git "
#(define-markup-command (gitCommand layout props cmd) (markup?)
   (let* ((result (string-split
                   (strsystem_internal (string-append "git " cmd))
                   #\newline)))
     (interpret-markup layout props
       #{ \markup \column #result #})))


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual commands

% Print the (short) committish of the latest commit
gitCommitish = \markup { \gitCommand "rev-parse --short HEAD" }

% Print the oneline commit message of the latest commit
gitCommit = \markup { \gitCommand "log --oneline HEAD^1..HEAD" }

% Print date and time of the latest commit
gitDateTime = \markup { \gitCommand "show -s --format=%ci HEAD" }

gitAuthor = \markup { \gitCommand "show -s --format=%an HEAD" }

gitEmail = \markup { \gitCommand "show -s --format=%ae HEAD" }

% Print the (short) committish of the latest commit
gitParentCommittish = \markup { \gitCommand "rev-parse --short HEAD^1" }

% Print the oneline commit message of the latest commit
gitParentCommit = \markup { \gitCommand "log --oneline HEAD~2..HEAD~1" }


% Print the branch the repository is currently on
gitBranch = \markup { \gitCommand "rev-parse --abbrev-ref HEAD" }

% Print the number of commits that lead to the
% current commit. This may not be reliable because
% of the counting of merge commits
gitRevisionNumber = \markup { \gitCommand "log --oneline | wc -l" }

% Return ##t if the repository is clean, i.e. if it
% doesn't have any uncommitted changes
#(define (gitIsClean)
   (eq? "" (strsystem_internal  "git status --porcelain")))

% gitIsClean determines whether the current repository is clean,
% but it only returns a boolean value. So if you want to display
% something based on this information you'll have to create the
% markup for yourself.
#(define-markup-command (gitIsCleanMarkup layout props yes-markup no-markup)
   (markup? markup?)
   (if (gitIsClean)
       (interpret-markup layout props
         #{ \markup #yes-markup #})
       (interpret-markup layout props
         #{ \markup #no-markup #})))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% "Verbose" commands

% Print the full commit message of the latest commit
gitFullCommit = \markup { \gitCommand "log --pretty=full HEAD^1..HEAD" }

% Print a full diff between current HEAD and the working tree
gitDiff = \markup { \gitCommand "diff" }

