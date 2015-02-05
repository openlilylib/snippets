\version "2.18.0"

%{
  Basic example demonstrating the general use of openLilyLib
%}

% Load the basic functionality
\include "openlilylib"

% Setting loglevel to 'log', otherwise one wouldn't see some instructional messages
\setOption global.loglevel #oll-loglevel-log

% Load the example library
% Notice (on the console) that actually example/__main__.ily is loaded
\loadModule "example"

#(ly:message "\nTry to load a module that is already loaded:")
\loadModule "example/load-test.ly"

#(ly:message "\nTry to load a non-existent module:")
\loadModule "example/this/is/not/a/module.ily"


#(ly:message "Use a command defined in the loaded modules")
\hello

#(ly:message "\nOverwrite one option, keep default of another, try to set a non-existent option.")
\setOption example.common.thickness 0.8
\setOption example.common.thin-thickness 0.5
#(ly:message (format "Default value of example.common.thick-thickness: ~a\n"
               #{ \getOption example.common.thick-thickness #}))

\displayOptions