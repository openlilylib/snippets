\version "2.18.0"

\include "scheme-lib/modules.ily"

#(use-modules (scheme-lib lalily parser-location))

\header {
  snippet-title = "include helper"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    \wordwrap {
      include files shortcuts
    }
  }
  tags = "include"
  status = "unfinished"
}


includePattern =
#(define-void-function (parser location idir pattern)(string? string?)
   (let ((dirname (string-append (location-extract-path location) idir)))

     (if (or (= (string-length dirname) 0)
             (not (eq? #\/ (string-ref dirname (- (string-length dirname) 1)))))
         (set! dirname (string-append dirname "/")))
     (if (or (not (file-exists? dirname)) (not (eq? 'directory (stat:type (stat dirname)))))
         (set! dirname #f))

     (if dirname (let* ((dir (opendir dirname))
                        (entry (readdir dir)))
                   (while (not (eof-object? entry))
                     (if (regexp-match? (string-match pattern entry))
                         (let ((file (string-append dirname entry)))
                           (ly:parser-include-string parser
                             (format "\\include \"~A\"\n" file))))
                     (set! entry (readdir dir))
                     )
                   (closedir dir)
                   ))
     ))

