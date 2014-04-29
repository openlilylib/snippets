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
    - Print full commit message
      The initial attempt doesn't work and only shows one line with the committish
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
% and only returns the first line of the result!
#(define (strsystem_internal cmd)
   (let* ((port (open-input-pipe cmd))
          (str (read-line port)))
     (close-pipe port)
     str))

#(define-markup-command (strsystem layout props cmd) (markup?)
   (interpret-markup layout props
     (strsystem_internal cmd)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual commands

% Print the (short) committish of the latest commit
gitCommitish = \markup { \strsystem "git rev-parse --short HEAD" }

% Print the oneline commit message of the latest commit
gitCommit = \markup { \strsystem "git log --oneline HEAD" }

% Print the full commit message of the latest commit
%TODO: This doesn't work yet
gitFullCommit = \markup { \strsystem "git log --pretty=full HEAD" }

% Print the branch the repository is currently on
gitBranch = \markup { \strsystem "git rev-parse --abbrev-ref HEAD" }

% Print the number of commits that lead to the
% current commit. This may not be reliable because
% of the counting of merge commits
gitRevisionNumber = \markup { \strsystem "git log --oneline | wc -l" }

% Return ##t if the repository is clean, i.e. if it
% doesn't have any uncommitted changes
#(define (gitIsClean)
   (eof-object? (strsystem_internal "git status --porcelain")))
