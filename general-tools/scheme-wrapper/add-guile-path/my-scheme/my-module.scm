; define a module for test purposes
(define-module (my-module))

; define a function that is available, if the module '(my-module) is used
(define-public (my-lambda . args)
  (display "my-lambda")(newline)
  (for-each (lambda (e) (display "\t")(display e)(newline)) args)
)
