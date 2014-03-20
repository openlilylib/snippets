\version "2.18.0"
#(use-modules (oop goops))

%%%%%%%%%%%%%
% class to store for example \set stanza = "1."

#(define-class <propset> ()
   (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
   (symbol #:accessor get-symbol #:setter set-symbol! #:init-keyword #:symbol)
   (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
   (previous #:accessor get-previous #:setter set-previous! #:init-value #f)
   (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
   )
% apply set to context
#(define-method (do-propset context (prop <propset>))
   (if (get-context prop)
       (let ((parctx (ly:context-find context (get-context prop))))
         (if (ly:context? parctx) (set! context parctx))))
   (set-previous! prop (ly:context-property context (get-symbol prop)))
   (ly:context-set-property! context (get-symbol prop) (get-value prop))
   )
%(export do-propset)
% apply unset to context
#(define-method (reset-prop context (prop <propset>))
   (if (get-context prop)
       (let ((parctx (ly:context-find context (get-context prop))))
         (if (ly:context? parctx) (set! context parctx))))
   (ly:context-set-property! context (get-symbol prop) (get-previous prop))
   )
%(export reset-prop)

% predicate
#(define-public (propset? p)(is-a? p <propset>))
% serialize to string
#(define-method (propset->string (ps <propset>))
   (format "~A\\set ~A = ~A" (if (is-once ps) "once " "") (string-append (if (get-context ps) (format "~A." (get-context ps)) "") (format "~A" (get-symbol ps))) (get-value ps)))
%(export propset->string)
% implement display
#(define-method (display (o <propset>) port) (display (propset->string o) port))

%%%%%%%%%%%%%

% store applyContext
#(define-class <apply-context> ()
   (proc #:accessor procedure #:setter set-procedure! #:init-keyword #:proc)
   )
% apply stored function to context
#(define-method (do-apply ctx (a <apply-context>))
   ((procedure a) ctx))
%(export do-apply)
% predicate
#(define-public (apply-context? a)(is-a? a <apply-context>))

% store overrides
#(define-class <override> ()
   (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
   (revert #:init-value #f #:accessor is-revert #:setter set-revert! #:init-keyword #:revert)
   (grob #:accessor get-grob #:setter set-grob! #:init-keyword #:grob)
   (prop #:accessor get-prop #:setter set-prop! #:init-keyword #:prop)
   (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
   (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
   )
% serialize to string
#(define-method (oop->string (o <override>))
   (let* ((ctxn (get-context o))
          (ctxp (if ctxn (format "~A." ctxn) "")))
     (if (is-revert o)
         (string-append "\\revert " ctxp (format "~A " (get-grob o)) (format "#'~A" (get-prop o)))
         (string-append (if (is-once o) "\\once " "") "\\override " ctxp (format "~A " (get-grob o)) (format "#'~A" (get-prop o)) " = " (format "~A" (get-value o)))
         )))
%(export oop->string)
% implement display
#(define-method (display (o <override>) port) (display (oop->string o) port))
% predicate
#(define-public (override? o)(is-a? o <override>))
% apply stored override to context
#(define-method (do-override ctx (mod <override>))
   (if (get-context mod)
       (let ((parctx (ly:context-find ctx (get-context mod))))
         (if (ly:context? parctx) (set! ctx parctx))))
   (ly:context-pushpop-property ctx (get-grob mod) (get-prop mod) (get-value mod)))
%(export do-override)
% apply revert to context
#(define-method (do-revert ctx (mod <override>))
   (if (get-context mod)
       (let ((parctx (ly:context-find ctx (get-context mod))))
         (if (ly:context? parctx) (set! ctx parctx))))
   (ly:context-pushpop-property ctx (get-grob mod) (get-prop mod)))
%(export do-revert)


