\version "2.17.10"

\header {
  oll-title = "Indicator for original line breaks in diplomatic editions."
  oll-short-description = ""
  oll-author = "Urs Liska"
  oll-description = \markup \wordwrap {
    Draw an indicator for original line breaks (but don't apply any break).
    It is a common notation to indicate line breaks in the
    original source (e.g. when editing sketches and drafts) with such marks.
    By default a dashed line is drawn above the staff.
  }
  oll-usage = \markup \wordwrap {
    \typewriter "\\diplomaticLineBreak" can be used at any rhythmic position.
    Without configuration it will print a dashed vertical line above the staff.
  }
  oll-options = #'((root-path . scholarly.diplomatic-line-break
                     ))
  oll-category = "editorial-marks"
  % add comma-separated tags to make searching more effective:
  oll-tags = "markup,line-breaks"
  oll-status = "ready"
  oll-todo = "Add more styles (dotted, arrows ...)"
}

\registerOption scholarly.diplomatic-line-break.thickness 1.6
\registerOption scholarly.diplomatic-line-break.dash-pattern #'(0.25 . 0.15)
\registerOption scholarly.diplomatic-line-break.length 3
\registerOption scholarly.diplomatic-line-break.direction #UP
\registerOption scholarly.diplomatic-line-break.Y-offset 0

diplomaticLineBreak = {
  \once \override Score.RehearsalMark.padding = #0
  \once \override Score.RehearsalMark.extra-offset = #`(0 . 
         ,#{ \getOption scholarly.diplomatic-line-break.Y-offset #})
  \once \override Score.RehearsalMark.direction = \getOption scholarly.diplomatic-line-break.direction
  \mark \markup {
    \override #`(on . ,(car #{ \getOption scholarly.diplomatic-line-break.dash-pattern #}))
    \override #`(off . ,(cdr #{ \getOption scholarly.diplomatic-line-break.dash-pattern #}))
    \override #`(thickness . ,#{ \getOption scholarly.diplomatic-line-break.thickness #})
    \draw-dashed-line #`(,0 . ,#{ \getOption scholarly.diplomatic-line-break.length #})
  }
}
