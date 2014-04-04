(use-modules
 (scheme-lib lalily parser-location)
 )

; make this file run once!
(if (not (and (defined? 'lalily-templates) lalily-templates))
    (begin
     (module-define! (current-module) 'lalily-templates #f)
     (load "store-init.scm")
     ))
(export lalily-templates)
(set! lalily-templates #t)
