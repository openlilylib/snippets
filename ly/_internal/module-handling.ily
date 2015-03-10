%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib/openlilylib                                 %
%              -----------                                                    %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2015                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Maintain a list of already loaded modules.
% Modules are only loaded once to avoid potentially expensive re-parsing
#(define oll-loaded-libraries '())
#(define oll-loaded-modules '())

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for type-checking of library options

% Simple regex check for Name plus email address in angled brackets:
% "Ben Maintainer <ben@maintainer.org>"
#(define (oll-maintainer? obj)
   (let ((pat (make-regexp ".*<.*@.*>")))
     (if (and (string? obj)
              (regexp-exec pat obj))
         #t #f)))

% Returns true for one maintainer or a list of them
#(define (oll-maintainers? obj)
   (or (oll-maintainer? obj)
       (and (list? obj)
            (every oll-maintainer? obj))))

% Returns true if obj is a string representation of an integer
#(define (integer-string? obj)
   (integer? (string->number obj)))

% Returns true if a string is a three-element dot-joined list of integers
#(define (oll-version-string? obj)
   (and (string? obj)
        (let ((lst (string-split obj #\.)))
          (and (= 3 (length lst))
               (every integer-string? lst)))))

% Alist with mandatory options for library declarations
% Each entry is a pair of option name symbol and type predicate
#(define oll-lib-mandatory-options
   `((maintainers . ,oll-maintainers?)
     (version . ,oll-version-string?)
     (short-description . ,string?)
     (description . ,string?)
     ))


% Declare a library, to be done in the __init__.ily file
% Arguments:
% - display-name: The official name of the library
% - name (optional): the directory name of the library
%   This name must be 'symbol?' compatible, i.e. must consist of
%   alphabetical characters and hyphens only.
%   This argument can be omitted if the display-name is the same
%   as the directory name with exception of capitalization.
%   (e.g. when the display-name is "ScholarLY" the implicit 'name'
%    is "scholarly").
% - options: a \with {} clause with metadata options.
%   some of them are mandatory, others can be used at the discretion
%   of the library maintainers:
%   Mandatory options:
%   - maintainers (string-or-alist?)
%     multiple maintainers can be maintained with name/email
%   - TO-BE-CONTINUED
%   Recognized options:
%   - TO-BE-DISCUSSED
%
declareLibrary =
#(define-void-function (parser location display-name name options)
   (string? (symbol?) ly:context-mod?)
   (let*
    ;; internal-name is either explicitly given
    ;; or the lowercase version of display-name
    ((internal-name
      (or name (string-downcase display-name)))
     ;; option path to the library's meta options
     (meta-path `(,(string->symbol internal-name) meta))
     ;; retrieve options from context mods
     (options
      (map (lambda (o)
             (cons (cadr o) (caddr o)))
        (ly:get-context-mods options))))

    ;; initialize library's meta option branch
    #{ \registerOption #meta-path #'() #}

    ;; check if all mandatory options are present
    (for-each
     (lambda (o)
       (let ((mand-opt (car o)))
         (if (not (assoc-ref options mand-opt))
             (oll:error (format "
    Missing option in library declaration!
    Library: \"~a\"
    Option: \"~a\"" display-name mand-opt) ""))
         ))
     oll-lib-mandatory-options)

    ;; process options, type-check mandatory options and store in meta
    (for-each
     (lambda (o)
       (let* ((opt-name (car o))
              (opt-val (cdr o))
              (predicate? (assoc-ref oll-lib-mandatory-options opt-name)))
         ;; check for type if there is a predicate (-> true for mandatory options)
         (if (and predicate?
                  (not (predicate? opt-val)))
             (oll:error (format "
    Type check failed for mandatory option in library declaration!
    Library: \"~a\"
    Option: \"~a\"
    Predicate: ~a" display-name opt-name predicate?) ""))

         ;; store option
         #{ \setChildOption #meta-path #opt-name #opt-val #}
         ))
     options)))


% Initialize a library before first use.
% This also serves as a kind of declaration of the intent of using it.
% If options are passed in a \with {} clause they are set after in
% initialization file has been loaded. If the initializiation did not
% register the options (in the form LIBRARY.OPTION) this will cause
% warnings about trying to set unregistered options.
useLibrary =
#(define-void-function (parser location options name)
   ((ly:context-mod?) symbol? )
   "Load an openLilyLib library and initialize it"
   (if (not (member name oll-loaded-libraries))
       ;; Determine paths to init and main files
       (let* ((lib-dir
               (string-append
                #{ \getOption global.root-path #}
                "/" (symbol->string name) "/"))
              (init-file
               (string-append lib-dir "__init__.ily"))
              (main-file
               (string-append lib-dir "__main__.ily")))

         ;; Create a root option for the library
         #{ \registerOption #(list name) #'() #}

         ;; Load initialization file if it exists
         (if (file-exists? init-file)
             (begin
              (oll:log location "Initialize library \"~a\" ..." name)
              (ly:parser-include-string parser (ly:gulp-file init-file))))

         ;; If a \with clause has been given pass the options to the library.
         ;; If the options have not been registered in the __init__ file this
         ;; will trigger oll:warn messages but don't abort the job.
         (if options
             (for-each
              (lambda (o)
                (let ((opt-path (list name (cadr o)))
                      (opt-val (caddr o)))
                  #{ \setOption #opt-path #opt-val #}))
              (ly:get-context-mods options)))

         ;; load the main file of the library or issue a warning if that isn't found.
         (if (file-exists? main-file)
             (begin
              (ly:parser-include-string parser (ly:gulp-file main-file))
              (set! oll-loaded-libraries
                    (append oll-loaded-libraries
                      `(,name)))
              (oll:log location "... completed."))
             (oll:warn location (format "Library main file \"~a\" not found" main-file))))))


% Conditionally register and load a library when
% for the first time a module from that library is requested.

%%% DEPRECATED !!!
%%% This is deprecated together with \loadModule
%%%
registerLibrary =
#(define-void-function (parser location lib)
   (string?)
   "Register a library with the configuration system
    if it hasn't been already loaded.
    If the library has an __init__.ily file
    this is loaded (library initialized) too."
   (if (not (member lib oll-loaded-libraries))
       (begin
        (set! oll-loaded-libraries
              (append oll-loaded-libraries
                `(,lib)))
        (let* ((root #{ \getOption global.root-path #})
               (lib-init-file (string-join
                               `(,root ,lib "__init__.ily") "/")))
          (if (file-exists? lib-init-file)
              (begin
               (oll:log "initialize library \"~a\"" lib)
               (ly:parser-include-string parser
                 (format "\\include \"~a\"" lib-init-file))))))))

% Load module from an openLilyLib library
% A module may be an individual file or a whole library, this can also be
% designed by the individual library.
% The string argument to be given is the path to the module, starting from
% the root directory of openLilyLib. It can be either an actual file or a
% directory name indicating the module (the check is whether the last item
% contains a dot in its name). If there's no dot in the last element of the
% path we assume it is a directory and try to load a file "__main__.ily"
% inside that directory.
loadModule =
#(define-void-function (parser location path)(string?)
   "Load an openLilyLib module if it has not been already loaded."
   (let*
    ((path-list (string-split path #\/))
     (lib (first path-list))
     (last-elt
      (if (string-index (last path-list) #\.)
          ;; if the last element is a file (with extension)
          ;; we don't do anything, otherwise we append the
          ;; default "module name"
          '()
          '("__main__.ily")))
     (append-path (string-join
                   (append path-list last-elt) "/"))
     (load-path (string-append
                 #{ \getOption global.root-path #}
                 "/"
                 append-path)))

    ;; DEPRECATION !!!
    ;; If used for loading a main library we should now use
    ;; \useLibrary instead
    (if (= 1 (length path-list))
        (oll:warn location
          "\n\\loadModule is deprecated for loading libraries.
Please use the more idiomatic and powerful \\useLibrary now."))

    ;; try to load the file if it isn't already present
    (if (member load-path oll-loaded-modules)
        (oll:log "module ~a already loaded. Skipping." load-path)
        (if (file-exists? load-path)
            (begin
             (oll:log "Registering library ~a" (first path-list))
             ;; first register/load the library
             #{ \registerLibrary #(first path-list) #}
             ;; then load the requested module
             (oll:log "load module ~a" load-path)
             (ly:parser-include-string parser
               (format "\\include \"~a\"" load-path))
             (set! oll-loaded-modules
                   (append! oll-loaded-modules `(,load-path))))
            (oll:warn "module not found: ~a" load-path)))))
