\version "2.18.0"

% Load the basic functionality
\include "openlilylib"

% Increase loglevel, otherwise one wouldn't see some instructional messages
#(set! oll-loglevel oll-loglevel-log)

% load a single file from module "test"
\loadModule "test/load-test.ly"

% The file is not parsed again
\loadModule "test/load-test.ly"

% Trying to load a non-existent file gives a warning but otherwise no problems
\loadModule "this/is/not/a/module.ily"

% Load the whole module "test"
% (This would load load-test.ly too, if that weren't already loaded above)
\loadModule "test"

% Use a command defined in the loaded modules
\hello
