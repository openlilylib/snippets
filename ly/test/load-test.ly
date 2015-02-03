
% Output a confirmation message that we are being parsed
#(ly:message "\nLoaded through openLilyLib!")

hello =
#(define-void-function (parser location)()
  (ly:message "\nHello,\nthis is printed by an included funtion.\n"))