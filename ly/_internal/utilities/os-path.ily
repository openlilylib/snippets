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

%{
  Provide tools to work with file paths.
  Mostly written by Jan-Peter Voigt
%}

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
               ))))


% append a relative path to the path the calling file is int
#(define (get-normalized-path locstring)
   (let* ((loclist (string-split (car locstring) #\/)) ; path split to list
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
     (if (string-index path-extra #\.)
         (normalize-path path-extra)
         path-extra)))

% Return the absolulte path/name of the file where this
% command has been called from (i.e. not necessarily
% the one that is currently compiled)
thisFile =
#(define-scheme-function (parser location)()
   "Return the absolute path to the current file"
   (car (ly:input-file-line-char-column location)))
