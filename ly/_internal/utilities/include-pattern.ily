\version "2.18.0"

\header {
  oll-title = "Include multiple files"
  oll-author = "Jan-Peter Voigt"
  oll-short-description = \markup {
    \wordwrap {
      Includes multiple files at once using
      a regular expression.
    }
  }
  oll-category = "general-tools"
  oll-tags = "include, file-handling"
  oll-status = "unfinished"
}

% Should includes be logged or not?
\registerOption internal.include-pattern.display-includes ##f

includePattern =
#(define-void-function (parser location idir pattern)
   (string? string?)
   (let ((dirname
          (if (absolute-path? idir)
              (string-append idir "/")
              (string-append (location-extract-path location) "/" idir)))
         (includefiles '())
         (pattern-regexp (make-regexp pattern)))

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
                 (let* ((ipv #{ \getOption internal.include-pattern.display-includes #})
                        (istr (format "\\include \"~A\"\n" file)))
                   (if (and ipv (or (not (list? ipv))(> (length ipv) 0)))
                       (oll:log "~A" istr))
                   (ly:parser-include-string parser istr)))
       includefiles)
     ))
