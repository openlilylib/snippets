%{
  Initialization file of the "example" library.
  __init__.ily files are used to initialized shared variables,
  set up the environment, register options for the library
%}

#(ly:message "Initializing example library.")

#(ly:message
  (format "\nThis message is triggered in file ~a"
    #{ \thisFile #}))


#(ly:message "\nDefine variable \"in-init\" in initialization file")

#(define in-init "defined in init")

#(ly:message "\nRegister some options in the __init__ file")

\registerOption example.common.thickness 1.2
\registerOption example.common.thick-thickness 2
