\version "2.17.96"
\include "alist-access.ily"


% work with a-lists
\clralist tags % equivalent to "tags=#'()" but can be called, whenever a void-function is allowed
% setalist wraps (assoc-set! <a-list> <symbol> <value>)
\setalist tags instrumentName "Trumpet (b)"
% addalist leaves the order of input in the assoc-list for the price of a little bit more computing
\addalist tags midiInstrument "trumpet"
% now the variable tags is defined and set, so display it ...
#(display tags)

#(newline)

% work with nested a-lists
\clratree opts % equivalent to "tags=#'()" but can be called, whenever a void-function is allowed
% set '((staff . ((trumpet . ((name . "Trumpet")) )) ))
\setatree opts staffs.trumpet.name "Trumpet"
% addatree leaves the order of input in the assoc-list for the price of a little bit more computing
\addatree opts staffs.trombone.name "Trombone"
\addatree opts staffs.trombone.clef "bass"
#(display opts)

% There is more to come, where I make excessive use of nested a-lists (or a-trees?) to parametresize options
% this might be called "[clr|set|add]Option" ???
