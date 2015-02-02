% Global option handling for openLilyLib
%
% Options are stored in one nested alist
% Managment of that alist is realized through the a-list-access.ily files.

% Global object holding all configuration data of all loaded openLilyLib modules
\clratree openlilylib-options

% Interface to store and retrieve options from one global option alist.
% The following functions are mere wrappers around functions defined in
% _/internal/utilities/alist-acces.ily.

% Set an option
% #1: Provide a tree path in dotted or list notation
%     the first item of the path is the library name,
%     followed by an arbitrary path at the library's discretion
% #2: Any Scheme value
setOllOption =
#(define-void-function (parser location opt-path val)
   (list? scheme?)
   #{ \setatree openlilylib-options #opt-path #val #})

% Retrieve an option
% Provied a tree path in dotted or list notation
% This has of course to match a set option.
getOllOption =
#(define-scheme-function (parser location opt-path)
   (list?)
   #{ \getatree openlilylib-options #opt-path #})

% TODO:
% Provide commands to bulk-process this.
% Maybe also make it possible to load options froma  JSON file
% (although I'm not completely sure if that JSON file would be
% actually easier to maintain).
