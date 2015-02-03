% Make general openLilyLib utilities available to any library.
% See TODO: DOC for more information
% This file is part of the openLilyLib library infrastructure
% ... TOBEDONE ...
%
% This file initializes openLilyLib

#(ly:set-option 'relative-includes #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Make common functionality available to all openLilyLib "users"
\include "utilities/__main__.ily"

% Logging capabilities with different log levels
\include "logging.ily"

% Common option handling
\include "options.ily"

% Set default loglevel to 'warning'
% (can only be done after options have been included)
\registerOllOption global.loglevel #oll-loglevel-warning

% Set the root path of openLilyLib
% - for oll module inclusion
% - for Scheme module inclusion
% This must be called from the main openlilylib file
% because that's inside the desired root directory
setRootPath =
#(define-void-function (parser location)()
   (let* ((path (get-normalized-path (ly:input-file-line-char-column location))))
     #{ \registerOllOption global.root-path #path #}
     (if (not (member path %load-path))
         (set! %load-path `(,path ,@%load-path)))))


% Functionality to load and manage modules
\include "module-handling.ily"


% Welcome message.
% This is a default ly:message because otherwise we'd have to mess around with
% loglevels. This shouldn't be logged anyway.

#(ly:message "\nopenLilyLib: library infrastructure successfully loaded.\n\n")
