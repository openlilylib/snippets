%{
  "main" file of the "example" library
  The main files should load the whole library.
%}

% Output a confirmation message that we are being parsed
#(ly:message "\nModule \"example\" loaded through openLilyLib!\n")
#(ly:message (format "I am ~a" #{ \thisFile #}))



#(ly:message "\nLoad a file from within the module.\n")
\loadModule "example/load-test.ly"

#(display
  (format "Use variable in __main__ that was defined in __init__: ~a" in-init))
