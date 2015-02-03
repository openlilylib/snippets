\version "2.18.2"

\header {
  snippet-title = "Get Current File Name"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    \wordwrap {
      Returns the absolute path to the file from which
      the function is called (i.e. not of the file that
      was passed to LilyPond.
    }
  }
  tags = "?"
  status = "ready"
}

thisFile =
#(define-scheme-function (parser location)()
   "Return the absolute path to the current file"
   (car (ly:input-file-line-char-column location)))
