\version "2.16.2"

\header {
  snippet-title = "Overriding stencils of objects with markups"
  snippet-author = "Janek Warchoł, David Nalesnik"
  snippet-source = "link to the mailing list archives or a website, if applicable"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "stencil, markup"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished, undocumented"
  % TODO:
  % - make it work without having to use quotation marks around grob name
  % - change syntax to \override Grob.stencil = stencil-from-markup markup ?
}

customStencilFromMarkup =
#(define-music-function (parser location name mrkup) (string? markup?)
   (let* ((name (string-regexp-substitute " " "" name)) ; remove any spaces
          (name-components (string-split name #\.))
          (context-name "Voice")
          (grob-name #f))

     (if (> 2 (length name-components))
         (set! grob-name (car name-components))
         (begin
          (set! grob-name (cadr name-components))
          (set! context-name (car name-components))))
     #{
       \override $context-name . $grob-name #'stencil =
       #(lambda (grob)
          (grob-interpret-markup
           grob mrkup))
     #}))

{
  \customStencilFromMarkup "Staff.TimeSignature" \markup \vcenter {
    \musicglyph #"timesig.mensural34"
    \musicglyph #"three"
  }
  \customStencilFromMarkup "NoteHead" \markup \vcenter { lol }
  \customStencilFromMarkup "Dots" \markup \vcenter { * }
  \time 6/1
  \clef "petrucci-f4"
  a\breve. g | f e |
}
