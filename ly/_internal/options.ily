% Global option handling for openLilyLib
%
% Options are stored in one nested alist
% Managment of that alist is realized through the a-list-access.ily files.

% Global object holding all configuration data of all loaded openLilyLib modules
\clratree openlilylib-options

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to store and retrieve options from one global option alist.
% The following functions rely on functionality in
% _/internal/utilities/alist-access.ily.

% A library can register options (best to be done in the __init__.ily file).
% Later end users can only set registered options, so this is kind of a syntax check.
%
% #1: option path in list or dot notation.
%     The first item should be the library name
% #2: initial value
%     If the user doesn't set the option explicitly this value is assumed
registerOption =
#(define-void-function (parser location opt-path init)
   (list? scheme?)
   #{ \setatree openlilylib-options #opt-path #init #})

% Set an option
% #1: Provide a tree path in dotted or list notation
%     the first item of the path is the library name,
%     followed by an arbitrary path at the library's discretion
% #2: Any Scheme value
setOption =
#(define-void-function (parser location opt-path val)
   (list? scheme?)
   (let (
          ;; test if the second-to-last branch contains
          ;; an entry for that option (assoc needed in order not to
          ;; stumble over existing entries with #f)
          (registered
           (assoc
            (last opt-path)
            #{ \getatree openlilylib-options
               #(list-head opt-path
                  (- (length opt-path) 1)) #})))
     (if registered
         (begin
          #{ \setatree openlilylib-options #opt-path #val #}
          (oll:log location "Option set: ~a"
            (format "~a: ~a"
              (dot-path->string opt-path) val)))
         (oll:warn location "Not a valid option path: ~a" (dot-path->string opt-path)))))

% Retrieve an option
% Provied a tree path in dotted or list notation
% Retrieving a non-existing option path issues a warning and returnes #f
getOption =
#(define-scheme-function (parser location opt-path)
   (list?)
   (let ((val #{ \getatree openlilylib-options #opt-path #}))
     (if val
         val
        (begin
         (oll:warn location "Try retrieving non-existent option: ~a" (dot-path->string opt-path))
         #f))))

% TODO:
% Provide commands to bulk-process this.
% Maybe also make it possible to load options froma  JSON file
% (although I'm not completely sure if that JSON file would be
% actually easier to maintain).
