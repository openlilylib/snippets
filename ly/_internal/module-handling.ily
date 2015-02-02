

% Maintain a list of already loaded modules.
% Modules are only loaded once to avoid potentially expensive re-parsing
#(define oll-loaded-libraries '())
#(define oll-loaded-modules '())


% Conditionally register and load a library when
% for the first time a module from that library is requested.
#(define (register-library lib)
   "Register a library with the configuration system
    if it hasn't been already loaded.
    If the library has an __init__.ily file
    this is loaded (library initialized) too."
   (if (not (member lib oll-loaded-libraries))
       (set! oll-loaded-libraries
             (append oll-loaded-libraries
               `(,lib)))
       (let ((lib-init-file (string-join
                             `(,openlilylib-root ,lib "__init__.ily") "/")))
         (if (file-exists? lib-init-file)
             (begin
              (oll:log "initialize library ~a" lib)
              (ly:parser-include-string parser
                (format "\\include \"~a\"" lib-init-file)))))))

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
     (load-path (string-append openlilylib-root "/" append-path)))
    ;; try to load the file if it isn't already present
    (if (member load-path oll-loaded-modules)
        (oll:log "module ~a already loaded. Skipping." load-path)
        (if (file-exists? load-path)
            (begin
             (oll:log "load module ~a" load-path)
             (ly:parser-include-string parser
               (format "\\include \"~a\"" load-path))
             (register-library (first path-list))
             (set! oll-loaded-modules
                   (append! oll-loaded-modules `(,load-path))))
            (oll:warn "module not found: ~a" load-path)))))
