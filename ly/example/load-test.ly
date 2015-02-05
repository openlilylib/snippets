
% Output a confirmation message that we are being parsed
#(ly:message "\nLoaded through openLilyLib!")

this-file = \thisFile

hello =
#(define-void-function (parser location)()
  (ly:message
   (format "\nHello,\nthis is printed by an included funtion
from file ~a\n" this-file)))