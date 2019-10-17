%{
  deprecate.ily
  use this file to point any user to moved files in openLilyLib.
  Do *not* simple remove the files you want to modify but instead:
  - clear the file
  - include this file (\include "deprecate.ily")
  - use its functions to
    - produce a meaningful error message
    - redirect the include to the new location (if possible)
%}

% Use this function when the whole file has moved to a new location inside /ly
% and if it can be used the same way there.
% #1: old name
%     can be an arbitrary string indicating as closely as possible which file
%     has been deprecated.
% #2: new-location
%     is the include path relative to /ly.
%     for users who haven't added /ly to their include path this is
%     prepended automatically
ollRedirect =
#(define-void-function (parser location old-name new-location)
   (string? string?)
   (ly:parser-include-string parser
     (format "\\include \"ly/~a\"" new-location))
   (ly:warning
    (format "

   openLilyLib file/module
      \"~a\"
   has been moved to a new location.
   Please update the \\include command to
      \\include \"~a\"
   and look into the new file, maybe you need to make further adjustments.
   For now the new file has been included anyway.
   Please also look at openLilyLib's documentation for setting up the new
   include path settings if you haven't done so already. The given path is
   relative to the \"/ly\" directory inside openLilyLib's root directory.
"
      old-name
      new-location)))

% Print a deprecation warning for those files that have been
% turned into openLilyLib package modules
ollSnippetsIncludeHint =
#(define-void-function (parser location loc)(ly:input-location?)
   (let
    ((file (car (ly:input-file-line-char-column (*location*)))))
    (ly:message "\n\nYou are directly invoking a file within openLilyLib/snippets.")
    (ly:message "This is deprecated.")
    (ly:message "Please change the include from 'definitions.ily' to 'module.ily'")
    (ly:message "or better invoke the file as a proper openLilyLib package module with")
    (ly:message "  \\include \"oll-core/package.ily\"")
    (ly:message "  \\loadModule snippets.path.to.module\n")
    (ly:message "For determining the proper module path please refer to the")
    (ly:message "filename of the loaded file, which is")
    (ly:message "  ~a\n\n" file)))

ollModuleDeprecation =
#(define-void-function (parser location old-module new-module)
   (string? string?)
    (ly:message "\n\nDEPRECATION!\n\n")
    (ly:message "You have loaded the openLilyLib module\n\n")
    (ly:message "  ~a\n\n" old-module)
    (ly:message "The functionality of this module has been moved")
    (ly:message "or reimplemented in the module\n\n")
    (ly:message "  ~a\n\n" new-module)
    (ly:message "Please consider switching to the new module.")
    (ly:message "Consult the documentation to check whether the new module")
    (ly:message "can be used in the same way as the old one.\n\n"))
