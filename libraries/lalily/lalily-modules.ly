\version "2.18.0"
% this code is copied from /general-tools/scheme-wrapper/add-guile-path/
% this way I can make this scheme-accessible

#(use-modules (ice-9 regex))
% get the working directory as a string-list
#(define-public (listcwd) '())
% test: is this path-string absolute?
#(define-public (absolutePath? path) #f)

% we have to check, if we are running on windows, because (getcwd) returns a path string with native sparators
% and on windows an absolute path starts with a letter and a colon - not a slash: 'C:\' vs. '/'
#(let* ((os (getenv "OS"))
        (isWindows (if (and (string? os) (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os)) #t #f))
        (wrx (if isWindows (make-regexp "^[a-z]:$" regexp/icase) #f)))
   ; listcwd: split (getcwd) with native path separator
   (set! listcwd (lambda () (string-split (getcwd)(if isWindows #\\ #\/))))
   ; absolutePath?: test if path list denotes an absolute path
   (set! absolutePath?
         (lambda (path)
           (if isWindows
               (if (and (> (length path) 0) (regexp-exec wrx (car path))) #t #f)
               (and (> (length path) 0) (= 0 (string-length (car path))))
               )))
   )

% add guile load path relative to this file
#(define-public addGuilePath
   (define-void-function (parser location folder)(string?)
     (let* ((locstring (ly:input-file-line-char-column location)) ; the file path of the location
             (loclist (string-split (car locstring) #\/)) ; path split to list
             ; if path list is not absolute, prefix current working directory
             ; then join to string with separator "/"
             (path-extra (let ((pl (if (absolutePath? loclist)
                                       loclist (append (listcwd) loclist))))
                           (string-join (reverse (cdr (reverse pl))) "/" 'infix)))
             ; normalize path-list: remove all entries ".." and "." and modify the list respectively
             (normalize-list (lambda (path)
                               (let ((ret '()))
                                 (for-each (lambda (e)
                                             (set! ret (cond ((equal? e "..")(if (> (length ret) 1) (cdr ret) '()))
                                                         ((equal? e ".") ret)
                                                         (else `(,e ,@ret))))) path)
                                 (reverse ret))))
             ; normalize path-string: remove all entries ".." and "."
             (normalize-path (lambda (s) (string-join (normalize-list (string-split s #\/)) "/" 'infix))))
       ; normalize path and add folder
       (set! path-extra (if (string? path-extra)
                            (string-append (if (eq? (string-ref path-extra 0) #\.)
                                               path-extra (normalize-path path-extra)) "/" folder)
                            folder))
       ; if path is not already in %load-path, add it
       (if (not (member path-extra %load-path)) (set! %load-path `(,path-extra ,@%load-path)))
       ;(display %load-path)
       )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% add the folder above as guile load path, so that we can (use-modules (lalily utilities))
\addGuilePath ".."
% import lalily utilities
#(use-modules (lalily utilities))
