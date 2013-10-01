\version "2.17.25"

\header {
  snippet-title = "Generalized offsetter"
  snippet-author = "David Nalesnik"
  snippet-description = \markup {
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
  tags = "offsets, offsetting, scheme"
  status = "unfinished"
  first-known-supported-version = "2.17.6"
}

%%%%%%%%%%%%%%%%%%%
%   THE SNIPPET   %
%%%%%%%%%%%%%%%%%%%

#(define (pair-list? x)
  (and (list? x)
       (every number-pair? x)))

#(define (offset-general arg offsets)
"Displace @code{arg} by @code{offsets} if @code{arg} is a number, a
number pair, or a list of number pairs.  If @code{offsets} is an empty
list or if there is a type-mismatch, @code{arg} will be returned."
  (cond
    ((and (number? arg) (number? offsets))
     (+ arg offsets))
    ((and (number-pair? arg)
          (or (number? offsets)
              (number-pair? offsets)))
     (coord-translate arg offsets))
    ((and (pair-list? arg) (pair-list? offsets))
     (map
       (lambda (x y) (coord-translate x y))
       arg offsets))
    (else arg)))

#(define ((offsetter property offsets) grob)
  (let* ((immutable (ly:grob-basic-properties grob))
         ; To obtain default values, we need the original key/value pair for our
         ; property from the basic-properties alist.  The tweak/override which
         ; calls our procedure has added a new pair for this property to the
         ; head of the alist.  We reverse the alist so our search will return
         ; the original entry, rather than the new one. If, however, our search
         ; returns an anonymous procedure, the property isn't an immutable
         ; property. (We've returned the entry added by the tweak/override.)
         (target (assoc-get property (reverse immutable)))
         ; calculate or read default values
         (vals
           (if (procedure? target)
               ; check for #<procedure #f (grob)>
               (if (procedure-name target)
                   (target grob)
                   '())
               target))
         (can-type-be-offset?
           (or (number? vals)
               (number-pair? vals)
               (pair-list? vals))))

    (if can-type-be-offset?
        ; '(+inf.0 . -inf.0) would offset to itself.  This will
        ; be confusing to user, so issue a warning.
        (if (equal? empty-interval vals)
            (ly:warning "default '~a of ~a is ~a and can't be offset"
              property grob vals)
            (let* ((orig (ly:grob-original grob))
                   (siblings
                     (if (ly:spanner? grob)
                         (ly:spanner-broken-into orig)
                         '()))
                   (total-found (length siblings))
                   ; since there is some flexibility in input syntax,
                   ; structure of `offsets' is normalized
                   (offsets
                     (if (or (not (pair? offsets))
                             (number-pair? offsets)
                             (and (pair-list? offsets) (pair-list? vals)))
                         (list offsets)
                         offsets)))

              (define (helper sibs offs)
                ; apply offsets to the siblings of broken spanners
                (if (pair? offs)
                    (if (eq? (car sibs) grob)
                        (offset-general vals (car offs))
                        (helper (cdr sibs) (cdr offs)))
                    vals))

              (if (>= total-found 2)
                  (helper siblings offsets)
                  (offset-general vals (car offsets)))))

            (begin
              (ly:warning "the property '~a of ~a cannot be offset" property grob)
              vals))))

offset =
#(define-music-function (parser location property offsets item)
  (symbol-list-or-symbol? scheme? symbol-list-or-music?)
  (_i "Offset the default value of @var{property} of @var{item} by
@var{offsets}.  If @var{item} is a string, the result is
@code{\\override} for the specified grob type.  If @var{item} is
a music expression, the result is the same music expression with an
appropriate tweak applied.")
  (if (ly:music? item)
      #{ \tweak #property #(offsetter property offsets) #item #}
      (if (check-grob-path item parser location
                                #:default 'Bottom
                                #:min 2
                                #:max 2)
          #{
            \override #item . #property =
              #(offsetter property offsets)
          #}
          (make-music 'Music))))

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
