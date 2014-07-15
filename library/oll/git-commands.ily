\version "2.16.2"

\header {
  snippet-title = "Print Git repository information"
  snippet-author = "Urs Liska, Lars Haulin"
  snippet-source = "http://lilypondblog.org/2014/01/why-use-version-control-for-engraving-scores/#comment-34076"
  snippet-description = \markup {
    This snippet provides a number of commands to print information
    about the current Git repository in scores. This can be used for
    clearly indicating from which version a file has been compiled.

    The snippet has been started from an example given as a comment
    on the Scores of Beauty blog (see 'snippet-source').
  }
  snippet-category = "project-management"
  % add comma-separated tags to make searching more effective:
  tags = "versioning,project history, archiving"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished, buggy"

  %NOTE: This currently works on Linux only

  %{
    TODO: complete a useful set of commands.
    Wishlist:
    - Autocommit on LilyPond run
      !!!Is that a useful function? Or rather a dangerous one?
    FIXME: Gracefully handle missing Git installation
           Gracefully handle the file not being in a repository
  %}
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% "Verbose" commands

% Print the full commit message of the latest commit
gitFullCommit = \markup { \gitCommand "log --pretty=full HEAD^1..HEAD" }

% Print a full diff between current HEAD and the working tree
gitDiff = \markup { \gitCommand "diff" }

