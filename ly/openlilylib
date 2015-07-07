% This is the main entry file for openLilyLib.
% When this is included different libraries and their modules can be loaded using
% the \import command defined here.

% Including this file also makes available some general helper material that
% is therefore automatically available for any contained library, for example the
% LilyPond version predicates that can be used to create code depending on the
% LilyPond version currently executed

% Include (once) the basic infrastructure of openLilyLib
%
% This does several things:
% - define a global variable 'openlilylib-root
%   which is the absolute path to the root of openLilyLib
%   (the folder this file is located in)
%   This can be used to construct paths to locations in the
%   libraries that are relative to openlilylib-root
% - add this directory to Scheme's module path. Scheme modules
%   can now be created and references from this root too.
% - Add module handling support (\loadModule and friends)
% - Add general tools that are available for all libraries.
%   - lilypond-version-predicates
%   - logging commands
%
% NOTE: There has been a major syntax change in 2.19.22:
% the parser argument is now obsolete with a number of functions,
% e.g. ly:parser-include-string.
% As the lilypond-version-predicates have not been included yet
% we have to hard-code the switch here.
initOpenLilyLib =
#(define-void-function (parser location)()
   (if (not (defined? 'openlilylib-options))
       (if (let ((v (ly:version)))
             (or (> (first  v)  2)
                 (> (second v) 19)
                 (>= (third v) 22)))
           (ly:parser-include-string
            "\\include \"_internal/init-openlilylib.ily\"")
           (ly:parser-include-string parser
             "\\include \"_internal/init-openlilylib.ily\"")
           )))
\initOpenLilyLib
