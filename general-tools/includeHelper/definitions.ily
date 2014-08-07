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
                       (let ((dirname (string-append (location-extract-path location) idir))
                             (includefiles '())
                             (pattern-regexp (make-regexp pattern))

                             (if (or (= (string-length dirname) 0)
                                     (not (eq? #\/ (string-ref dirname (- (string-length dirname) 1)))))
                               (set! dirname (string-append dirname "/")))
                             (if (or (not (file-exists? dirname))
                                     (not (eq? 'directory (stat:type (stat dirname)))))
                               (set! dirname #f))

                             (if dirname (let ((dir (opendir dirname)))
                                           (do ((entry (readdir dir) (readdir dir))) ((eof-object? entry))
                                             (if (regexp-exec pattern-regexp entry)
                                               (set! includefiles (merge includefiles (list (string-append dirname entry)) string>?))))
                                           (closedir dir)))

                             (for-each (lambda (file)
                                         (let* ((ipv (ly:parser-lookup parser 'includePatternVerbose))
                                                (istr (format "\\include \"~A\"\n" file)))
                                           (if (and ipv (or (not (list? ipv))(> (length ipv) 0)))
                                             (ly:message "~A" istr))
                                           (ly:parser-include-string parser istr)))
                                       includefiles)
                             ))
