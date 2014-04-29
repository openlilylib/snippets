\version "2.17.10"

\header {
  snippet-title = "Indicator for original line breaks"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    This snippet draws a dashed vertical line above the staff.
    This is a common notation to indicate line breaks in the
    original source (e.g. when editing sketches and drafts).
  }
  % add comma-separated tags to make searching more effective:
  tags = "editorial,markup,line breaks"
  status = "ready"
  %{ TODO:
     Make appearance of line configurable through variables
     Optionally: Add more styles (dotted, arrows ...)
  %}
}

lineBreakMark = {
  \once \override Score.RehearsalMark.padding = #0
  \mark \markup {
    \override #'(on . 0.25)
    \override #'(off . 0.15)
    \override #'(thickness . 1.6)
    \draw-dashed-line #'(0 . 3)
  }
}
