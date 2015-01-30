
% Output a confirmation message that we are being parsed
#(ly:message "\nLoaded through openLilyLib!")

hello =
#(define-void-function (parser location)()
  (display "\nHello,\nthis is printed by an included funtion.\n"))