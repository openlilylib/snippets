\version "2.18.2"
\include "oll-core/package.ily"
\loadModule snippets.notation-snippets.shaping-bezier-curves.shape-tie-column
%\include "definition.ily"

{
  % Modifying the first and third ties in a chord. () is shorthand for no change
  <e' bes' es''>1 ~
  \shapeTieColumn #'(
                      ( ((0 . 1) (0 . 1) (0 . 1) (0 . 1)) )
                      ( () )
                      ( ((0 . -2) (0 . -1) (0 . -1) (0 . -1)) ) )
  q
  \break

  % Modifying the broken ties for a chord.  Only the second tie before the
  % break is modified, but all three are after.
  q ~
  \shapeTieColumn #'(
                      ( () ((0 . 1) (0 . 1) (0 . 1) (0 . 1)) )
                      ( ((0 . 0) (0 . 2) (0 . 2) (0 . 0))
                        ((0 . -0.1) (0.5 . -0.4) (1.5 . -0.4) (2 . -0.1)) )
                      ( () ((0 . -2) (0 . -1) (0 . -1) (0 . -1)) ) )
  \break
  q
}

\layout {
  indent = 0
  ragged-right = ##t
}