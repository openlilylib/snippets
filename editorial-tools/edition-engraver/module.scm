;;;; This file is part of the lalily section in openlilylib
;;;;
;;;; Copyright (C) 2011--2015 Jan-Peter Voigt <jp.voigt@gmx.de>
;;;;
;;;; lalily is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; lalily is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with lalily.  If not, see <http://www.gnu.org/licenses/>.


(define-module (editorial-tools edition-engraver module))

(use-modules
 (lily)
 (oop goops)
 (srfi srfi-1)
 (scheme-lib lalily utilities)
 (scheme-lib lalily storage)
 (templates lalily store)
 )



; class to store for example \set stanza = "1."
(define-class <propset> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (symbol #:accessor get-symbol #:setter set-symbol! #:init-keyword #:symbol)
  (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
  (previous #:accessor get-previous #:setter set-previous! #:init-value #f)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )
; apply set to context
(define-method (do-propset context (prop <propset>))
  (if (get-context prop)
      (let ((parctx (ly:context-find context (get-context prop))))
        (if (ly:context? parctx) (set! context parctx))))
  (set-previous! prop (ly:context-property context (get-symbol prop)))
  (ly:context-set-property! context (get-symbol prop) (get-value prop))
  )
(export do-propset)
; apply unset to context
(define-method (reset-prop context (prop <propset>))
  (if (get-context prop)
      (let ((parctx (ly:context-find context (get-context prop))))
        (if (ly:context? parctx) (set! context parctx))))
  (ly:context-set-property! context (get-symbol prop) (get-previous prop))
  )
(export reset-prop)

; predicate
(define-public (propset? p)(is-a? p <propset>))
; serialize to string
(define-method (propset->string (ps <propset>))
  (format "~A\\set ~A = ~A" (if (is-once ps) "once " "") (string-append (if (get-context ps) (format "~A." (get-context ps)) "") (format "~A" (get-symbol ps))) (get-value ps)))
(export propset->string)
; implement display
(define-method (display (o <propset>) port) (display (propset->string o) port))



; store applyContext
(define-class <apply-context> ()
  (proc #:accessor procedure #:setter set-procedure! #:init-keyword #:proc)
  )
; apply stored function to context
(define-method (do-apply ctx (a <apply-context>))
  ((procedure a) ctx))
(export do-apply)
; predicate
(define-public (apply-context? a)(is-a? a <apply-context>))




; store overrides
(define-class <override> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (revert #:init-value #f #:accessor is-revert #:setter set-revert! #:init-keyword #:revert)
  (grob #:accessor get-grob #:setter set-grob! #:init-keyword #:grob)
  (prop #:accessor get-prop #:setter set-prop! #:init-keyword #:prop)
  (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )
; serialize to string
(define-method (oop->string (o <override>))
  (let* ((ctxn (get-context o))
         (ctxp (if ctxn (format "~A." ctxn) "")))
    (if (is-revert o)
        (string-append "\\revert " ctxp (format "~A " (get-grob o)) (format "#'~A" (get-prop o)))
        (string-append (if (is-once o) "\\once " "") "\\override " ctxp (format "~A " (get-grob o)) (format "#'~A" (get-prop o)) " = " (format "~A" (get-value o)))
        )))
(export oop->string)
; implement display
(define-method (display (o <override>) port) (display (oop->string o) port))
; predicate
(define-public (override? o)(is-a? o <override>))
; apply stored override to context
(define-method (do-override ctx (mod <override>))
  (if (get-context mod)
      (let ((parctx (ly:context-find ctx (get-context mod))))
        (if (ly:context? parctx) (set! ctx parctx))))
  (ly:context-pushpop-property ctx (get-grob mod) (get-prop mod) (get-value mod)))
(export do-override)
; apply revert to context
(define-method (do-revert ctx (mod <override>))
  (if (get-context mod)
      (let ((parctx (ly:context-find ctx (get-context mod))))
        (if (ly:context? parctx) (set! ctx parctx))))
  (ly:context-pushpop-property ctx (get-grob mod) (get-prop mod)))
(export do-revert)

; stored edition tags
(define-public (editions) #f)
; set edition tags
(define-public (set-editions! ed) #f)
; add edition modification
(define-public (add-edmod parser edition takt pos path mod) #f)
; create edition engraver with path
(define-public (edition-engraver tag-path) #f)
; call proc with arg edition-engraver for all active
(define-public (walk-edition-engravers proc) #f)
; display all stored modifications
(define-public (display-mods) #f)
; display all registered edition-engraver paths
(define-public (display-edition) #f)

; find edition-engraver in this or any parent context
(define-public (context-find-edition-engraver context) #f)

(define lalily:edition-tags 'lalily:edition-tags)
; now actually implement the needed functions and the edition-engraver itself
(let ((mod-tree (tree-create 'mods)) ; a singleton to store all edition mods
       (edition-list '())
       (edition-tree (tree-create 'edition)) ; a tree with all edition-engravers
       (context-count (tree-create 'context))) ; a tree to count all edition-engravers by tag-path
  (define (o->sym o) (cond ((symbol? o) o) ((string? o) (string->symbol o)) (else (string->symbol (format "~A" o)))))
  (set! editions (lambda () (if (list? edition-list) edition-list '())))
  (set! set-editions! (lambda (eds) (if (list? eds) (set! edition-list eds) (ly:error "list expected: ~A" eds))))
  (set! add-edmod
        (lambda (parser edition takt pos path modm)
          (let* ((edition (if (string? edition) (string->symbol edition) edition))
                 (path `(,edition ,takt ,pos ,@path))
                 (mods (tree-get mod-tree path)))
            (if (not (list? mods)) (set! mods '()))
            (cond
             ((ly:music? modm)
              (let ((x 0))
                (define (add-mods modmus ctx)
                  (for-some-music
                   (lambda (m)
                     (cond
                      ((eq? 'ContextSpeccedMusic (ly:music-property m 'name))
                       (let* ((ct (ly:music-property m 'context-type))
                              (elm (ly:music-property m 'element)))
                         (if (eq? 'Bottom ct)
                             #f
                             (begin
                              (add-mods elm ct)
                              #t)
                             )
                         ))
                      ((eq? 'OverrideProperty (ly:music-property m 'name))
                       (let* ((once (ly:music-property m 'once #f))
                              (grob (ly:music-property m 'symbol))
                              (prop (ly:music-property m 'grob-property))
                              (prop (if (symbol? prop)
                                        prop
                                        (car (ly:music-property m 'grob-property-path))))
                              (value (ly:music-property m 'grob-value))
                              (mod (make <override> #:once once #:grob grob #:prop prop #:value value #:context ctx)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((eq? 'RevertProperty (ly:music-property m 'name))
                       (let* ((grob (ly:music-property m 'symbol))
                              (prop (ly:music-property m 'grob-property))
                              (prop (if (symbol? prop)
                                        prop
                                        (car (ly:music-property m 'grob-property-path))))
                              (mod (make <override> #:once #f #:revert #t #:grob grob #:prop prop #:value #f #:context ctx)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((eq? 'PropertySet (ly:music-property m 'name))
                       (let* ((once (ly:music-property m 'once #f))
                              (symbol (ly:music-property m 'symbol))
                              (value (ly:music-property m 'value))
                              (mod (make <propset> #:once once #:symbol symbol #:value value #:context ctx)))
                         (set! mods `(,@mods ,mod))
                         #t
                         ))
                      ((eq? 'ApplyContext (ly:music-property m 'name))
                       (let* ((proc (ly:music-property m 'procedure))
                              (mod (make <apply-context> #:proc proc)))
                         (set! mods `(,@mods ,mod))
                         ))
                      ((or
                        (eq? 'OttavaMusic (ly:music-property m 'name))
                        )
                       ; TODO which music types can be made available this way? (take care of \once!)
                       (set! mods `(,@mods ,(context-mod-from-music parser m)))
                       #t
                       )
                      ((or
                        (eq? 'TextScriptEvent (ly:music-property m 'name))
                        (eq? 'LineBreakEvent (ly:music-property m 'name))
                        (eq? 'PageBreakEvent (ly:music-property m 'name))
                        (eq? 'PageTurnEvent (ly:music-property m 'name))
                        (eq? 'ApplyOutputEvent (ly:music-property m 'name))
                        (eq? 'MarkEvent (ly:music-property m 'name))

                        (eq? 'PartCombineForceEvent (ly:music-property m 'name))
                        (eq? 'ExtenderEvent (ly:music-property m 'name))
                        (eq? 'HyphenEvent (ly:music-property m 'name))
                        )
                       (set! mods `(,@mods ,m))
                       #t
                       )
                      (else #f)
                      )
                     )
                   modmus))
                (add-mods modm #f)))
             ((ly:context-mod? modm)(set! mods `(,@mods ,modm)))
             )
            (tree-set! mod-tree path mods)
            #f
            )))
  (set! edition-engraver
        (lambda (tag-path . props)
          (let ((eng #f)
                (cmf (if (eq? #t tag-path) (get-music-folder)))) ; current music folder
            (define (get-sym c)(string->symbol (base26 c)))
            (set! eng (lambda (context)
                        (let* ((tag-path tag-path)
                               (tag '())
                               (barnum 0)
                               (measurepos (ly:make-moment 0 1))
                               (ctxid (ly:context-id context))
                               (ctxname (ly:context-name context))

                               ; TODO get-paths -> collect from all paths
                               (get-paths
                                (lambda (edition takt pos)
                                  (if (and (string? ctxid)(> (string-length ctxid) 0))
                                      (let ((ctxid (string->symbol ctxid)))
                                        `(
                                           (,edition ,takt ,pos ,@tag-path)
                                           (,edition ,takt ,pos ,@tag-path ,ctxid)
                                           (,edition ,takt ,pos ,@tag-path ,ctxname)
                                           (,edition ,takt ,pos ,@tag-path ,ctxname ,ctxid)
                                           (,edition ,takt ,pos ,@tag)
                                           ))
                                      `(
                                         (,edition ,takt ,pos ,@tag-path ,ctxname ,ctxid)
                                         (,edition ,takt ,pos ,@tag-path ,ctxname)
                                         (,edition ,takt ,pos ,@tag)
                                         )
                                      )))

                               (initialize
                                (lambda (trans)
                                  (if (procedure? tag-path) (set! tag-path (tag-path)))
                                  (if (not (list? tag-path))

                                      ; TODO edition-id
                                      (let ((edition-id (ly:context-property context 'edition-id #f))
                                            (parent (ly:context-parent context))
                                            (peng #f))

                                        (define (search-peng path eng)
                                          (if (eqv? (object-property eng 'context) parent)
                                              (set! peng eng)))

                                        (ly:message "edition-id: ~A" edition-id)
                                        (if (and (not (list? tag-path)) (list? edition-id) (> (length edition-id) 0))
                                            (set! tag-path edition-id)
                                            (begin
                                             (if (ly:context? parent) (walk-edition-engravers search-peng))
                                             (if peng (set! tag-path (object-property peng 'tag-path)))
                                             (if (not (list? tag-path))
                                                 (set! tag-path (if (list? cmf) cmf (get-music-folder))))
                                             ))))
                                  (let* ((cn (ly:context-name context))
                                         (cid (ly:context-id context))
                                         (path `(,@tag-path ,(o->sym cn)))
                                         (ccid (tree-get context-count path)))
                                    (define (topctx context)
                                      (let ((par (ly:context-find context 'Score)))
                                        (if (ly:context? par) (topctx par) context)))
                                    (if (not (integer? ccid))(set! ccid 0))
                                    (tree-set! context-count path (+ ccid 1))
                                    ; (ly:message "~A ~A" ccid path)
                                    (set! path `(,@path ,(get-sym ccid)))
                                    (set! tag path)
                                    (tree-set! edition-tree path
                                      (cons eng
                                        (let* ((c context)
                                               (takt (ly:context-property c 'currentBarNumber))
                                               (mpos (ly:context-property c 'measurePosition)))
                                          (cons takt mpos) )))
                                    (set-object-property! eng 'context context)
                                    (set-object-property! eng 'tag-path tag-path)
                                    (set-object-property! eng 'path path)
                                    (set-object-property! eng 'tag-path-idx ccid)

                                    ; (if (lalily:verbose) (ly:message "looking for editions in ~A" (glue-list path "/")))
                                    )))
                               ; paper column interface
                               (paper-column-interface
                                (lambda (engraver grob source-engraver)
                                  (let ((takt (ly:context-property context 'currentBarNumber))
                                        (pos (ly:context-property context 'measurePosition)))
                                    (if (eq? #t (ly:grob-property grob 'non-musical))
                                        (for-each
                                         (lambda (edition)
                                           (for-each
                                            (lambda (path)
                                              (let ((mods (tree-get mod-tree path)))
                                                (if (list? mods)
                                                    (for-each
                                                     (lambda (mod)
                                                       (cond
                                                        ((and (ly:music? mod) (eq? 'LineBreakEvent (ly:music-property mod 'name)))
                                                         (set! (ly:grob-property grob 'line-break-permission) (ly:music-property mod 'break-permission)))
                                                        ((and (ly:music? mod) (eq? 'PageBreakEvent (ly:music-property mod 'name)))
                                                         (set! (ly:grob-property grob 'page-break-permission) (ly:music-property mod 'break-permission)))
                                                        ((and (ly:music? mod) (eq? 'PageTurnEvent (ly:music-property mod 'name)))
                                                         (set! (ly:grob-property grob 'page-turn-permission) (ly:music-property mod 'break-permission)))
                                                        ((and (ly:music? mod) (eq? 'ApplyOutputEvent (ly:music-property mod 'name)))
                                                         (let ((proc (ly:music-property mod 'procedure)))
                                                           (proc grob context context)
                                                           ))
                                                        )) mods))))
                                            (get-paths edition takt pos))) (editions)))
                                    )))
                               (start-translation-timestep
                                (lambda (trans . recall) ; recall from process-music
                                  (let ((takt (ly:context-property context 'currentBarNumber))
                                        (pos (ly:context-property context 'measurePosition))
                                        (modc '()))
                                    (define (modc+ mod)(set! modc `(,@modc ,mod)))
                                    (set! barnum takt)(set! measurepos pos)
                                    (for-each (lambda (edition)
                                                (for-each
                                                 (lambda (path)
                                                   (let ((mods (tree-get mod-tree path)))
                                                     ;(display path)(display mods)(newline)
                                                     (if (list? mods)
                                                         (for-each (lambda (mod)
                                                                     (cond
                                                                      ((override? mod)
                                                                       (if (is-revert mod)
                                                                           (do-revert context mod)
                                                                           (do-override context mod))
                                                                       (modc+ mod))
                                                                      ((propset? mod)
                                                                       (do-propset context mod)
                                                                       (modc+ mod))
                                                                      ((apply-context? mod)
                                                                       (do-apply context mod))
                                                                      ((ly:context-mod? mod)
                                                                       (ly:context-mod-apply! context mod)
                                                                       (modc+ mod))
                                                                      )) mods)
                                                         )
                                                     )) (get-paths edition takt pos))) (editions))
                                    ; warning if start-translation-timestep is not called in first place
                                    (if (and (> (length modc) 0)(> (length recall) 0) (eq? #t (car recall)))
                                        (begin
                                         (ly:warning "missing @ ~A ~A ~A" takt pos (glue-list tag "/"))
                                         (for-each (lambda (mod) (ly:warning "---> ~A" mod)) modc)
                                         ))
                                    )))
                               (stop-translation-timestep
                                (lambda (trans)
                                  (let ((takt (ly:context-property context 'currentBarNumber))
                                        (pos (ly:context-property context 'measurePosition)))
                                    (for-each (lambda (edition)
                                                (for-each
                                                 (lambda (path)
                                                   (let ((mods (tree-get mod-tree path)))
                                                     (if (list? mods)
                                                         (for-each (lambda (mod)
                                                                     (cond
                                                                      ((and (override? mod)(is-once mod))
                                                                       (do-revert context mod))
                                                                      ((and (propset? mod)(is-once mod))
                                                                       (reset-prop context mod))
                                                                      ))
                                                           mods))
                                                     )) (get-paths edition takt pos))) (editions))
                                    )))

                               (process-music
                                (lambda (trans)
                                  (let ((takt (ly:context-property context 'currentBarNumber))
                                        (pos (ly:context-property context 'measurePosition)))
                                    ; recall start-translation-timestep, if it is not called already
                                    (if (or (not (equal? takt barnum))(not (equal? measurepos pos)))
                                        (start-translation-timestep trans #t))
                                    (for-each (lambda (edition)
                                                (for-each
                                                 (lambda (path)
                                                   (let ((mods (tree-get mod-tree path)))
                                                     (if (list? mods)
                                                         (for-each (lambda (mod)
                                                                     (cond
                                                                      ((and (ly:music? mod) (eq? 'TextScriptEvent (ly:music-property mod 'name)))
                                                                       (let ((grob (ly:engraver-make-grob trans 'TextScript '()))
                                                                             (text (ly:music-property mod 'text))
                                                                             (direction (ly:music-property mod 'direction #f)))
                                                                         (ly:grob-set-property! grob 'text text)
                                                                         (if direction (ly:grob-set-property! grob 'direction direction))
                                                                         ))
                                                                      ((and (ly:music? mod) (eq? 'MarkEvent (ly:music-property mod 'name)))
                                                                       (let ((grob (ly:engraver-make-grob trans 'RehearsalMark '()))
                                                                             (text (ly:music-property mod 'label)))
                                                                         (if (not (markup? text))
                                                                             (let ((rmi (ly:context-property context 'rehearsalMark))
                                                                                   (rmf (ly:context-property context 'markFormatter)))
                                                                               (if (and (integer? rmi)(procedure? rmf))
                                                                                   (let ((rmc (ly:context-property-where-defined context 'rehearsalMark)))
                                                                                     (set! text (rmf rmi rmc))
                                                                                     (ly:context-set-property! rmc 'rehearsalMark (+ 1 rmi))
                                                                                     ))))
                                                                         (ly:grob-set-property! grob 'text text)
                                                                         ))
                                                                      ))
                                                           mods))
                                                     )) (get-paths edition takt pos))) (editions))
                                    )))
                               (finalize
                                (lambda (trans)
                                  (if (eq? 'Score (ly:context-name context))
                                      (let* ((takt (ly:context-property context 'currentBarNumber))
                                             (pos (ly:context-property context 'measurePosition))
                                             (parser (ly:assoc-get 'parser props #f #f)))
                                        (ly:message "(~A) finalize ~A (~A ~A)"
                                          (glue-list (editions) ", ")
                                          (glue-list tag "/")
                                          takt (if (ly:moment? pos) (moment->string pos) pos))
                                        (if parser
                                            (let* ((outname (ly:parser-output-name parser))
                                                   (logfile (format "~A.edition.log" outname)))
                                              (ly:message "writing '~A' ..." logfile)
                                              (with-output-to-file logfile
                                                (lambda()
                                                  (display-edition)
                                                  (display "<--- mods --->")(newline)
                                                  (display-mods)
                                                  ))
                                              ))
                                        (set! context-count (tree-create 'context))
                                        ))))
                               )
                          `(
                             (initialize . ,initialize)
                             (acknowledgers
                              (paper-column-interface . ,paper-column-interface)
                              )
                             (start-translation-timestep . ,start-translation-timestep)
                             (stop-translation-timestep . ,stop-translation-timestep)
                             (process-music . ,process-music)
                             (finalize . ,finalize)
                             ))))
            eng)))
  (set! walk-edition-engravers
        (lambda (proc)
          (tree-walk edition-tree '() ; walk all
            (lambda (path key value)
              (proc path (if (pair? value) (car value) value))
              ) '(empty . #f) '(sort . #f))
          ))

  (set! context-find-edition-engraver
        (lambda (context)
          (let ((peng #f))
            (define (search-peng path eng)
              (if (eqv? (object-property eng 'context) context)
                  (set! peng eng)))
            (if (ly:context? context) (walk-edition-engravers search-peng))
            peng
            )))

  (set! display-edition (lambda () (tree-display edition-tree
                                     '(pathsep . ".")
                                     '(valuesep . " ")
                                     `(vformat . ,(lambda (p) (let ((m (if (pair? p) (cdr p) p)))
                                                                (if (and (pair? m)(ly:moment? (cdr m)))
                                                                    (format "(~A . ~A)" (car m)(moment->string (cdr m)))
                                                                    (format "~A" m))
                                                                )))
                                     )))
  (set! display-mods
        (lambda ()
          (tree-display mod-tree
            '(pathsep . " ")
            `(pformat . ,(lambda (v) (cond
                                      ((ly:moment? v) (moment->string v))
                                      (else (format "~A" v))
                                      )))
            `(vformat . ,(lambda (v)
                           (if (list? v)
                               (glue-list (map (lambda (e)
                                                 (cond
                                                  ((ly:music? e)
                                                   (format "[M] ~A" (ly:music-property e 'name))
                                                   )
                                                  (else (format "~A" e)))) v) "\n") (format "~A" v)))))))
  )

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (frac-or-mom? v) (or (fraction? v)(ly:moment? v)))
(define (music-or-contextmod? v) (or (ly:music? v)(ly:context-mod? v)))

;%%% the key function "editionMod"
(define-public editionMod
  (define-void-function (parser location edition takt pos path mod)
    (string-or-symbol? integer? frac-or-mom? list? music-or-contextmod?)
    "Add modification to edition @ measure moment"
    (if (fraction? pos)(set! pos (ly:make-moment (car pos)(cdr pos))))
    (add-edmod parser edition takt pos (create-music-path #f path) mod)
    ))


(define-public (memom? v)
  (or (integer? v)
      (and (pair? v)(integer? (car v))
           (let ((cv (cdr v)))
             (if (list? cv)(set! cv (car cv)))
             (or (rational? cv)(frac-or-mom? cv))
             ))))
(define (limemom? v)(and (list? v)(every memom? v)))

;%%% the key function "editionMMod"
(define-public editionMMod
  (define-void-function (parser location edition mposl path mod)
    (string-or-symbol? limemom? list? music-or-contextmod?)
    "Add modification to edition at all positions in mposl"
    (for-each
     (lambda (p)
       (let ((takt (car p))
             (pos (cdr p)))
         (if (list? pos)(set! pos (car pos)))
         (if (fraction? pos)(set! pos (fraction->moment pos)))
         (if (rational? pos)
             (set! pos (ly:make-moment (numerator pos)(denominator pos))))
         (add-edmod parser edition takt pos (create-music-path #f path) mod)
         )) mposl)
    ))

;%%% the key function "editionModList"
(define (limemom? v)(and (list? v)(every memom? v)))
(define-public editionModList
  (define-void-function (parser location edition path mod mposl)
    (string-or-symbol? list? music-or-contextmod? limemom?)
    "Add modification to edition at all positions in mposl"
    (let ((path (create-music-path #f path)))
      (for-each
       (lambda (p)
         (begin
          (if (integer? p)(set! p (list p 0)))
          (let ((takt (car p))
                (pos (cdr p)))
            (if (integer? pos)(set! pos (list pos 0)))
            (if (list? pos)(set! pos (car pos)))
            (if (fraction? pos)(set! pos (fraction->moment pos)))
            (if (rational? pos)
                (set! pos (ly:make-moment (numerator pos)(denominator pos))))
            (add-edmod parser edition takt pos path mod)
            ))) mposl)
      )))



(define (list-or-boolean? v) (or (boolean? v)(list? v)(procedure? v)))

;%%% the key function "editionEngraver"
(define-public editionEngraver
  (define-scheme-function (parser location tag)(list-or-boolean?)
    (edition-engraver tag `(parser . ,parser))))


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% the key function "addEdition"

; activate edition
(define-public addEdition
  (define-void-function (parser location edition)(string-or-symbol?)
    "Add edition to edition-list.
Every edition from the global edition-list will be listened for by the edition-engraver."
    (if (string? edition) (set! edition (string->symbol edition)))
    (if (not (memq edition (editions))) (set-editions! `(,@(editions) ,edition)))
    ))

;%%% the key function "removeEdition"

; deactivate edition
(define-public removeEdition
  (define-void-function (parser location edition)(string-or-symbol?)
    "Remove edition from edition-list.
Every edition from the global edition-list will be listened for by the edition-engraver."
    (if (string? edition) (set! edition (string->symbol edition)))
    (set-editions! (delete edition (editions)))
    ))

;%%% the key function "setEditions"

; set editions
(define-public setEditions
  (define-void-function (parser location editions)(list?)
    "Set edition-list to editions.
Every edition from the global edition-list will be listened for by the edition-engraver.
This will override the previously set list."
    (set-editions! (map (lambda (edition)
                          (cond
                           ((symbol? edition) edition)
                           ((string? edition) (string->symbol edition))
                           (else (string->symbol (format "~A" edition)))
                           )) editions))
    ))

