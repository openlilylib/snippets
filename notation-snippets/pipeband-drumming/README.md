From https://github.com/kastdeur/lilydrum

To make a pipeband drum staff use 
"\new PipeBandDrumStaff "

It accepts everything a drumstaff accepts.

Music is defined in \drummode. 
You can use every regular drum character, as "d" and "g" are appended to the list.


Embellishments (Side)
-------------
Flams and Drags can be added using "\flam" and "\drag". They can determine whether to be left or right by themselves for simple expressions.
If a certain hand is needed they can be inserted directly by appending "d" or "g" to "flam","drag",..
A "\flam d" is equal to "\flamd d"

There are currently 4 shortcuts:
\flam - A Flam
\drag - A Drag
\ruff - A Ruff
\sruff - A Swiss Ruff

appending dr to the above shortcuts triggers the unison bracket.

Flourishing (Tenor)
-------------
A few flourishes have been added, most are from the EUSPBA:
\splitTheFeather
\cartWheel
\up
\blfy	- butterfly
\rblfy	- reverse butterfly
\rthrow	- throw right
\lthrow	- throw left
\bthrow	- throw both
\rpush	- push right
\lpush	- push left
\bpush	- push both
\andrewStop	- St. Andrew's Stop
\stop

Scoops have not been created yet





