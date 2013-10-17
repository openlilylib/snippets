\version "2.17.6"

\include "generalized-offsets.ily"

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
