\version "2.17.6"

\include "generalized-offsets.ily"

\header {
  title = "Generalized offsetter"
  composer = "David Nalesnik"
  tags = "offsets, offsetting, scheme"
  status = "unfinished"
  first-known-supported-version = "2.17.6"
}

\markup {
  \vspace #2
    \wordwrap {
      The ability to offset default values of various properties would be a
      useful enhancement of LilyPond.  Currently, this is possible for the
      property "\'control-points" using the "\\shape" command.  The following
      snippet seeks to generalize the application of offsets to grob properties.
      Both overrides and tweaks are supported.  Offsets are currently limited to
      three data types: number, number-pair, and number-pair-list (the latter
      is defined by this snippet and represents the type used, for example, by
      "\'control-points)."  Offsets will work with many properties, but not all.
      (For example, offsets are limited to immutable grob properties; these are
      listed in "`scm/define-grobs.scm\'.)" Offsets are reckoned against default
      values.  It is not possible to accumulate offsets, nor to offset against
      a user-defined procedure.
    }
  }
\markup \vspace #2
  
%%%%%%%%%%%%%%%%%%%%%%
%   USAGE EXAMPLES   %
%%%%%%%%%%%%%%%%%%%%%%

\layout {
  ragged-right = ##t
  indent = 0
}

\relative c' {
  %% ARPEGGIO %%
  % default
  <c e g b>1\arpeggio
  <c e g b>1-\offset #'positions #'(-1 . 1) \arpeggio
  \bar "||"

  %% BREATHING SIGN %%
  % default
  c1 \breathe
  c1
  \once \offset #'Y-offset #1 BreathingSign
  \breathe
  \bar "||"

  %% DYNAMICS %%
  % default
  c1\f
  \once \offset #'X-offset #-1 DynamicText
  c1\f
  % DynamicLineSpanner
  c1-\offset #'padding #1 \f
  \bar "||"

  %% BEAMS %%
  % default
  c'8 d e f
  \once \offset #'positions #'(-1 . -1) Voice.Beam
  c8 d e f
  % same effect as an offset of '(-2 . -2)
  \once \offset #'positions #-2 Beam
  c8 d e f
  \override Beam #'breakable = ##t
  c8-\offset #'positions #'((-1 . -3) (-3 . -1)) [ d e f
  \break
  g8 f e d] c-\offset #'beam-thickness #0.48 [ d e f]
  \bar "||"

  %% TEXT SPANNERS %%
  c4\startTextSpan d e f\stopTextSpan
  \once \offset #'dash-fraction #'(0.1 0.3) TextSpanner
  \once \offset #'staff-padding #'(1.0 2.0) TextSpanner
  c4\startTextSpan d e f
  \break
  c4 d e f\stopTextSpan
  \bar "||"

  %% SLURS %%
  % this duplicates the effect of the \shape command
  \offset #'control-points #'(
                              ((0 . 0) (0 . 1) (0 . 2) (0 . 1))
                              ((1 . 0) (0 . 4) (0 . 4) (0 . 0))
                              ) Slur
  c4-\offset #'line-thickness #'(0 10) ( d e f
  \break
  c4 d e f)
  \bar "||"

  %% A warning is issued if user attempts to offset a property
  %% which has a value defaulting to '(+inf.0 . -inf.0)
  %{
  \break
  \offset #'extra-spacing-width #'(1 . 10) Score.MetronomeMark
  \tempo "Adagio"
  c1
  %}
}
