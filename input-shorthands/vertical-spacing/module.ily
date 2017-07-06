\version "2.16.2"

\header {
  snippet-title = "Shorthand for Setting Vertical Spacing"
  snippet-author = "Joram Berger"
  snippet-source = ""
  snippet-description = \markup {
    This snippet provides the simplespace function. Usage:
    "\simplespace basic-distance min-distance padding stretchability"
  }
  tags = "vertical spacing, shorthand, distance"
  status = "ready"
}

% this is intended to return the scheme construct with the given values:
simplespace =
#(define-scheme-function
  (parser location bdist mdist padd stret)
  (number? number? number? number?)
  `((basic-distance . ,bdist)
    (minimum-distance . ,mdist)
    (padding . ,padd)
    (stretchability . ,stret))
  )
