\version "2.18.0"

\header {
  snippet-title = "read multiline copmment as string"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    \wordwrap {
      Read a multiline comment, following this command, as string.
      This can be used to get a string in a foreign syntax
      to be given to an external process, which processes this (for example) to an EPS-file.
    }
  }
  tags = "comment,multiline-comment"
  status = "unknown"
}

#(use-modules
  (ice-9 rdelim)
  (ice-9 regex)
  )

% read text from multiline comment %{ %}
#(define-public (read-comment port linenr)
   (let ((rstart (make-regexp "^[^%]*%\\{(.*)$"))
         (rend (make-regexp "^(.*)%}.*$")))
     (define (collect lc status . lines)
       (let ((line (read-line port 'concat)))
         (if (string? line)
             (cond
              ((< lc linenr)
               (apply collect (+ lc 1) 0 lines))
              ((= status 0)
               (let ((match (regexp-exec rstart line)))
                 (if (regexp-match? match)
                     (let ((i (match:start match 1)))
                       (apply collect (+ lc 1) 1 (append lines (list (substring line i))))
                       )
                     (apply collect (+ lc 1) 0 lines)
                     )))
              ((= status 1)
               (let ((match (regexp-exec rend line)))
                 (if (regexp-match? match)
                     (let ((i (match:start match 1)))
                       (apply collect (+ lc 1) 2 (append lines (list (match:substring match 1))))
                       )
                     (apply collect (+ lc 1) 1 (append lines (list line)))
                     )))
              (else (apply string-append lines))
              )
             (apply string-append lines))
         ))
     (collect 1 0)
     ))

% scheme function to read comment: \readComment
readComment =
#(define-scheme-function (parser location)()
   (let* ((fll (ly:input-file-line-char-column location))
          (file (car fll))
          (linenr (cadr fll))
          (port (open-file file "r")))
     (read-comment port linenr)
     ))


