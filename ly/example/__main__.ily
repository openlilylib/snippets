%{
  "main" file of the "example" library
  The main files should load the whole library.
%}

% Output a confirmation message that we are being parsed
#(ly:message "\nModule \"example\" loaded through openLilyLib!\n")
#(ly:message (format "I am ~a" #{ \thisFile #}))



#(ly:message "\nLoad a file from within the module.\n")
\loadModule "example/load-test.ly"

#(ly:message "I don't understand why the following (display) fails.
Obviously the __init__.ily file is only parsed *after* __main__.ily")
#(display in-init)
