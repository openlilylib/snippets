\version "2.18.0"
\include "definitions.ily"

\addGuilePath my-scheme
% the first entry should be the absolute path to the folder "my-scheme" next to this file
#(display %load-path)

#(newline)

% load file from %load-path
#(load-from-path "my-init.scm")

% import/activate my-module, which is located in the folder "my-scheme"
#(use-modules (my-module))
% use the function, defined in module '(my-module)
#(my-lambda 'is 'a 'great 'procedure!)


