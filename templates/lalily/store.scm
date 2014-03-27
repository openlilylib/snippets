;;;; This file is part of lalily, an extension to lilypond <http://www.lilypond.org/>.
;;;;
;;;; Copyright (C) 2011--2012 Jan-Peter Voigt <jp.voigt@gmx.de>
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

(define-module (templates lalily store))

(use-modules
 (lily)
 (scheme-lib lalily utilities)
 (scheme-lib lalily registry)
 (scheme-lib lalily storage)
 (templates lalily definitions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; path substitution

(define (reg-path p) `(,@lalily:store:path-variables ,(object->symbol p)))
(define (map-name name)
  (let ((name (object->string name display)))
    (set! name
          (if (string-prefix? "LY_" name) (string-append "$" (substring name 3)) name)
          )
    ;(ly:message name)
    name
    ))
(define-public (register-path name path)
  (let ((name (map-name name)))
    (if (and (eq? #\$ (string-ref name 0)) (list? path))
        (set-registry-val (reg-path name) path)
        (if (list? path)
            (ly:warning "path substitutes have to start with '$' or 'LY_'! (~A)" name)
            (ly:warning "path is not a list: ~A" path))
        )))

(define-public (unfold-path path uncyc)
  (let ((pemp '()))
    (if (not (list? uncyc)) (set! uncyc (list)))
    (for-each (lambda (p)
                (let ((n (map-name p)))
                  (if (and (eq? #\$ (string-ref n 0)) (not (memq n uncyc)))
                      (let ((subst (get-registry-val (reg-path n) #f)))
                        (set! pemp (append pemp (if (list? subst) (unfold-path subst (cons n uncyc)) (list p)))))
                      (set! pemp (append pemp (list p)))
                      ))) path)
    pemp))
(define-public registerPath
  (define-void-function (parser location name path)(string-or-symbol? list?)
    (let ((name (map-name name)))
      (if (not (eq? #\$ (string-ref name 0)))
          (set! name (string-append "$" name)))
      (register-path name path)
      )))

(register-path '$UP '(..))
(register-path '$EMPTY '())
(register-path '$LOCAL '())
(define-public LY_NOOP '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store music in a tree

(define-public (put-music path music) #f)
(define-public (get-music path location) #f)
(define-public (has-music path dur location) #f)
(define-public (load-music path . location) #f)
(define-public (store-music path . location) #f)
(define-public (get-music-deep path skey defm location) #f)
(define-public (collect-music path pred) #f)
(define-public (get-music-keys path location) #f)
(define-public (has-music? path) #f)
(define-public (display-music-pieces) #f)
(define-public (display-template-ref path location . o) #f)
(define (add-template-ref tmpl) #f)
(let ((music-tree (tree-create 'music))
      (template-doc (tree-create 'template-doc)))
  (define (rel-path a b)
    (if (and (> (length a) 0) (> (length b) (length a)))
        (if (equal? (car a) (car b))
            (rel-path (cdr a) (cdr b))
            #f)
        b))
  (set! add-template-ref
        (lambda (path type)
          (let ((t (get-current-template)))
            (if (and (list? t)(> (length t) 0))
                (let ((tt (tree-get template-doc t))
                      (val type)
                      (pp (rel-path (get-current-music) path)))
                  (if (or (not (list? pp)) (eq? 'template type)) (set! pp path))
                  (if (tree? tt)
                      (set! val (tree-get tt pp))
                      (begin
                       (set! tt (tree-create 'template-ref))
                       (tree-set! template-doc t tt)))
                  (if (not val) (set! val type))
                  (tree-set! tt pp val)
                  ))
            )))
  (set! put-music (lambda (path music)
                    (let ((m (store-music path music)))
                      (if (ly:music? m) (set! music m)))
                    (tree-set! music-tree path music)))
  (set! get-music (lambda (path location)
                    (load-music path location)
                    (let ((p (tree-get music-tree path)))
                      (add-template-ref path 'need)
                      (if (ly:music? p)
                          (ly:music-deep-copy p)
                          (begin
                           (if location (ly:input-message location "unknown music '~A'" (glue-list path "/"))
                               (ly:message "unknown music '~A'" (glue-list path "/")))
                           (make-music 'SequentialMusic 'void #t))
                          )
                      )))

  (set! has-music (lambda (path dur location)
                    (load-music path location)
                    (let* ((bdur (cond
                                  ((ly:moment? dur) dur)
                                  ((ly:music? dur) (ly:music-length dur))
                                  ((list? dur) (ly:music-length (get-music dur location)))
                                  (else (ly:make-moment 0 0))))
                           (mus (tree-get music-tree path))
                           (mdur (if (ly:music? mus) (ly:music-length mus)(ly:make-moment 0 0))))
                      (ly:moment<? bdur mdur))))

  (set! load-music (lambda (path . location)
                     (let ((m (tree-get music-tree path))
                           (cbs (get-registry-val lalily:get-music-load-callbacks '())))
                       (define (search cbs)
                         (cond
                          ((ly:music? m) #t)
                          ((> (length cbs) 0)
                           (let ((cb (car cbs)))
                             (if (procedure? cb) (cb path))
                             (set! m (tree-get music-tree path))
                             (cond
                              ((ly:music? m) #t)
                              ((> (length cbs) 1) (search path (cdr cbs)))
                              (else #f)
                              )))
                          (else #f)))
                       (search cbs)
                       )))
  (set! store-music (lambda (path music)
                      (let ((cbs (get-registry-val lalily:get-music-store-callbacks '())))
                        (for-each (lambda (cb) (let ((m (cb path music)))
                                                 (if (ly:music? m) (set! music m))
                                                 )) cbs)
                        music
                        )))

  (set! get-music-deep (lambda (path skey defm location)
                         (let ((p (tree-get-from-path music-tree path skey #f)))
                           (add-template-ref (list skey) (if (ly:music? defm) 'deep-optional 'deep))
                           (if (ly:music? p) (ly:music-deep-copy p)
                               (if (ly:music? defm)
                                   defm
                                   (begin
                                    (if location (ly:input-message location "unknown music '~A' on path '~A'" skey (glue-list path "/"))
                                        (ly:message "unknown music '~A' on path '~A'" skey (glue-list path "/")))
                                    (make-music 'SequentialMusic 'void #t)))
                               )
                           )))
  (set! collect-music (lambda (path pal pred)
                        (let ((NULL #f))
                          (define (col pi po mc)
                            (let ((mus (tree-get music-tree (pal po))))
                              (if (and (ly:music? mus)(pred path mus))
                                  (set! mc (append mc (list (ly:music-deep-copy mus)))))
                              (if (> (length pi) 0)
                                  (col (cdr pi) (append po (list (car pi))) mc)
                                  mc)
                              ))
                          (col path '() '())
                          )))
  (set! get-music-keys (lambda (path location)
                         (let ((ret (tree-get-keys music-tree path)))
                           (if (list? ret) (reverse ret) '())
                           )))
  (set! has-music? (lambda (path)
                     (add-template-ref path 'optional)
                     (ly:music? (tree-get music-tree path))))

  (set! display-music-pieces (lambda ()
                               (tree-display music-tree `(vformat .
                                                           ,(lambda (v)
                                                              (if (ly:music? v)
                                                                  (let ((mom (ly:music-length v)))
                                                                    (format "~A/~A" (ly:moment-main-numerator mom)
                                                                      (ly:moment-main-denominator mom)))
                                                                  "*"))) )))
  (set! display-template-ref (lambda ()
                               (tree-walk-branch template-doc '()
                                 (lambda (path k val)
                                   (format #t "[Template] ~A" (glue-list path "/"))
                                   (if (tree? val) (begin
                                                    (newline)
                                                    (tree-display val)
                                                    ))
                                   (newline)
                                   ) '(sort . #t) `(sortby . ,(lambda (p1 p2) (string-ci<? (format "~A" (car p1)) (format "~A" (car p2))))) '(empty . #f))
                               ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store templates in a tree

(define-macro (make-template code)
  `(define-music-function
    (parser location piece options)
    (list? list?)
    ,code))

(define-public (register-template name music) #f)
(define-public (get-template name) #f)
(define-public (call-template name parser location piece options) #f)
(define-public (display-templates) #f)

(define-public (get-current-music) #f)
(define-public (display-music-stack) #f)
(define-public (get-current-template) #f)
(define-public (display-template-stack) #f)

(let* ((templ-tree (tree-create 'template))
       (empty-function (define-music-function (parser location piece options)(list? list?)
                         (get-music piece location)
                         ))
       (call-music-stack (stack-create))
       (call-template-stack (stack-create))
       (call-template-extra '())
       )
  (set! register-template (lambda (name fun)
                            (tree-set! templ-tree name fun) ))
  (set! get-template (lambda (name location)
                       (let* ((f (tree-get templ-tree name))
                              (error (lambda () (if location
                                                    (ly:input-message location "unknown template '~A'" name)
                                                    (ly:message "unknown template '~A'" name)))))
                         (if (not (ly:music-function? f))(set! f
                                                               (begin (error) empty-function)))
                         f)))
  (set! call-template (lambda (name parser location piece options)
                        (let ((tmpl (get-template name location)))
                          (add-template-ref name 'template)
                          (if (ly:music-function? tmpl)
                              (let ((mus #f))
                                (stack-push call-template-stack name)
                                (stack-push call-music-stack piece)
                                (set! mus ((ly:music-function-extract tmpl) parser location piece options))
                                (stack-pop call-music-stack)
                                (stack-pop call-template-stack)
                                mus)
                              (make-music 'SimultaneousMusic 'void #t))
                          )))

  (set! get-current-music (lambda () (stack-get call-music-stack)))
  (set! display-music-stack (lambda () (display call-music-stack)))
  (set! get-current-template (lambda () (stack-get call-template-stack)))
  (set! display-template-stack (lambda () (display call-template-stack)))

  (set! display-templates (lambda ()
                            (tree-display templ-tree `(vformat . ,(lambda (v) "*")) )))
  )
(define-public (create-template-path tabs path)
  (let* (
          (pabs (lambda (path) (and (list? path)(> (length path) 0)(member (car path) '(/ $ROOT LY_ROOT)))))
          (_tabs (or tabs (pabs path)))
          (path (if (and (not tabs)(pabs path)) (cdr path) path))
          (tabs _tabs)
          )
    (if tabs path
        (let ((cpart (get-current-template)))
          (normalize-path (if (list? cpart)(append cpart path) path))))))

(define-public (create-music-path mabs path)
  (let* (
          (path (if (list? path) (unfold-path path '()) '()))
          (pabs (lambda (path) (and (list? path)(> (length path) 0)(member (car path) '(/ $ROOT LY_ROOT)))))
          (_mabs (or mabs (pabs path)))
          (path (if (and (not mabs)(pabs path)) (cdr path) path))
          (mabs _mabs)
          )
    (normalize-path
     (unfold-path
      (if mabs path
          (let ((cpart (get-current-music)))
            ; TODO when does this happen?
            (if (not (list? cpart)) (set! cpart (get-music-folder)))
            (if (list? cpart)(append cpart path) path)
            )) '() ))
    ))

(define-public musicPath (define-scheme-function (parser location path)(list?)(create-music-path #f path)))
(define-public templatePath (define-scheme-function (parser location path)(list?)(create-template-path #f path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store music options
;;; create music folder

(define-public (get-music-folder) #f)
(define-public (set-music-folder! piece) #f)

(define-public (set-default-template piece tmpl options) #f)
(define-public (get-default-template piece location) #f)
(define-public (get-default-options piece location) #f)
(define-public (get-default-options-cumul piece location) #f)

(define-public (display-default-music) #f)

(let ((templates (tree-create 'defaults))
      (current-piece '()))
  (set! get-music-folder (lambda () current-piece))
  (set! set-music-folder! (lambda (piece) (set! current-piece piece)))
  (set! set-default-template (lambda (piece tmpl options)
                               (set! current-piece piece)
                               (tree-set! templates piece (cons tmpl options))))
  (set! get-default-template (lambda (piece location)
                               (let ((p (tree-get templates piece)))
                                 (if (pair? p) (car p) '(NOTFOUND)))))
  (set! get-default-options (lambda (piece location)
                              (let ((p (tree-get templates piece)))
                                (if (pair? p) (cdr p) '()))))
  (set! get-default-options-cumul (lambda (piece location)
                                    (let ((oplist (tree-collect templates piece (stack-create)))
                                          (opts '()))
                                      (for-each (lambda (p)
                                                  (let* ((o (if (pair? p)(cdr p) '()))
                                                         (ohead (ly:assoc-get 'header opts '() #f))
                                                         (nhead (ly:assoc-get 'header o '() #f))
                                                         (head (assoc-set-all! ohead nhead)))
                                                    (set! opts (assoc-set-all! opts o))
                                                    (if (> (length head))(assoc-set! opts 'header head))
                                                    )) (reverse oplist))

                                      opts
                                      )))
  (set! display-default-music (lambda ()
                                (tree-display templates `(vformat . ,(lambda (v)
                                                                       (format "[~A]\n~A" (glue-list (car v) "/") (format-alist (cdr v))))) )))
  )

(define-public (set-default-option parser location piece field value)
  (let ((tmpl (get-default-template piece location))
        (opts (get-default-options piece location)))
    (set-default-template piece tmpl (assoc-set! opts field value))
    ))
(define-public (remove-default-option parser location piece field)
  (let ((tmpl (get-default-template piece location))
        (opts (get-default-options piece location)))
    (set-default-template piece tmpl (assoc-remove! opts field))
    ))


(define-public (set-default-header parser location piece field value)
  (let* ((tmpl (get-default-template piece location))
         (opts (get-default-options piece location))
         (header (ly:assoc-get 'header opts '() #f)))
    (set! header (assoc-set! header field value))
    (set! opts (assoc-set! opts 'header header))
    (set-default-template piece tmpl opts)
    ))
(define-public (remove-default-header parser location piece field)
  (let* ((tmpl (get-default-template piece location))
         (opts (get-default-options piece location))
         (header (ly:assoc-get 'header opts '() #f)))
    (set! header (assoc-remove! header field))
    (set! opts (assoc-set! opts 'header header))
    (set-default-template piece tmpl opts)
    ))

(define-public (get-default-header name field . default)
  (let* ((def (if (>= (length default) 1) (car default) #f))
         (hopts (if (> (length default) 1) (cdr default) '()))
         (opts (if (ly:assoc-get 'inherit hopts) (get-default-options-cumul name #f) (get-default-options name #f)))
         (header (ly:assoc-get 'header opts '() #f)))
    (define (getv fl)
      (let ((ret (if (> (length fl) 0) (assoc-get (car fl) header #f #f) def)))
        (if (markup? ret) ret (if (> (length fl) 1) (getv (cdr fl)) def))))
    (if (list? field)
        (getv field)
        (ly:assoc-get field header def #f))
    ))
(define-public (get-music-folder-header-field field . default)
  (apply get-default-header (get-music-folder) field default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store quotes

(define (quotable-music mus)(and (ly:music? mus) (ly:moment<? (ly:make-moment 0 0) (ly:music-length mus))))
(define-public (quote-name path)(glue-list path ":"))
(define-public (track-quote path location) #f)
(define-public (is-quote path) #f)
(define-public (display-quotes) #f)
(define-public (add-tracked-quotes parser location) #f)
(let ((quotes (tree-create 'quotes)))
  (set! track-quote (lambda (path location) (let ((loclst (tree-get quotes path)))
                                              (tree-set! quotes path (if (list? loclst) (append loclst (list location)) (list location))))))
  (set! is-quote (lambda (path) (and (list? (tree-get quotes path)) (> (length (tree-get quotes path))))))
  (set! add-tracked-quotes (lambda (parser location)
                             (tree-walk quotes '()
                               (lambda (path key val)
                                 (let ((quey (quote-name path))
                                       (mus (get-music path (car val))))
                                   (if (quotable-music mus)
                                       (begin
                                        (if (lalily:verbose) (ly:message "add-quotable ~A" quey))
                                        (add-quotable parser quey
                                          (music-filter
                                           (lambda (event)
                                             (let ( (eventname (ly:music-property  event 'name))
                                                    (ret #t) )
                                               (for-each (lambda (n) (set! ret (and ret (not (eq? eventname n)))))
                                                 '())
                                               ret
                                               ))
                                           mus))
                                        ) ))) ) ))
  (set! display-quotes (lambda () (tree-display quotes `(vformat . ,(lambda (loclst)
                                                                      (glue-list (map (lambda (loc) (let ((lp (ly:input-file-line-char-column loc)))
                                                                                                      (format "~A ~A:~A" (car lp)(cadr lp)(caddr lp) ))) loclst) ", "))) )))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store paper defs

(define-public (register-paper name paper) #f)
(define-public (get-paper name) #f)
(define-public (display-paper-defs) #f)

(let ((paper-tree (tree-create 'paper)))
  (set! register-paper (lambda (name paper)
                         (tree-set! paper-tree name paper)))
  (set! get-paper (lambda (name)
                    (let* ((st (stack-create))
                           (cl (tree-collect paper-tree name st)))
                      (if (> (length cl) 0) (car cl) (ly:make-output-def) ))))
  (set! display-paper-defs (lambda () (tree-display paper-tree `(vformat . ,(lambda (pap) "*")) )))
  )
(define-public registerPaper (define-music-function (parser location name paper)(list? ly:output-def?)
                               (register-paper name paper)
                               (make-music 'SequentialMusic 'void #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store layout defs

(define-public (register-layout name layout) #f)
(define-public (get-layout name) #f)
(define-public (display-layout-defs) #f)

(let ((layout-tree (tree-create 'layout)))
  (set! register-layout (lambda (name layout)
                          (tree-set! layout-tree name layout)))
  (set! get-layout (lambda (name)
                     (tree-get layout-tree name)))
  (set! display-layout-defs (lambda () (tree-display layout-tree `(vformat . ,(lambda (pap) "*")) )))
  )
(define-public registerLayout (define-music-function (parser location name layout)(list? ly:output-def?)
                                (register-layout name layout)
                                (make-music 'SequentialMusic 'void #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store midi defs

(define-public (register-midi name midi) #f)
(define-public (get-midi name) #f)
(define-public (display-midi-defs) #f)

(let ((midi-tree (tree-create 'midi)))
  (set! register-midi (lambda (name midi)
                        (tree-set! midi-tree name midi)))
  (set! get-midi (lambda (name)
                   (tree-get midi-tree name)))
  (set! display-midi-defs (lambda () (tree-display midi-tree `(vformat . ,(lambda (pap) "*")) )))
  )
(define-public registerMidi (define-music-function (parser location name midi)(list? ly:output-def?)
                              (register-midi name midi)
                              (make-music 'SequentialMusic 'void #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store page templates

(define-public (register-page-template name scfunc) #f)
(define-public (get-page-template name) #f)
(define-public (call-page-template name parser location options) #f)
(define-public (display-page-templates) #f)

(let ((page-template-tree (tree-create 'page-template)))
  (set! register-page-template (lambda (name page-template)
                                 (tree-set! page-template-tree name page-template)))
  (set! get-page-template (lambda (name)
                            (tree-get page-template-tree name)))
  (set! call-page-template (lambda (name parser location options)
                             (let ((tmpl (get-page-template name)))
                               (if (ly:music-function? tmpl)
                                   (let ((bookpart #f))
                                     (set! bookpart ((ly:music-function-extract tmpl) parser location options))
                                     (if ly:book? bookpart #{ \bookpart { } #} )
                                     )
                                   #{ \bookpart { } #})
                               )))
  (set! display-page-templates (lambda () (tree-display page-template-tree `(vformat . ,(lambda (pap) "*")) )))
  )
(define-public registerPageTemplate (define-music-function (parser location name page-template)(list? ly:music-function?)
                                      (register-page-template name page-template)
                                      (make-music 'SequentialMusic 'void #t)))

