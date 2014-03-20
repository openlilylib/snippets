;;;; This file is part of the lalily section in openlilylib
;;;;
;;;; Copyright (C) 2011--2014 Jan-Peter Voigt <jp.voigt@gmx.de>
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


(define-module (lalily utilities))

(use-modules (lily)(oop goops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some helper functions

(define-public (base26 i)
  "produce a string A, B, ..., Z, AA, AB, ... for numbers
usable to allow 2.17+ list input like in: \\editionMod notes.sop.Voice.A
ATTENTION: there will be no ZZ but YZ -> AAA and YZZ -> AAAA"
  (let ((A (char->integer (if (< i 0) #\a #\A)))
        (i (if (< i 0) (- -1 i) i)))

    (define (baseX x i)
      (let ((q (quotient i x))
            (r (remainder i x)))
        (if (and (> q 0) (< q x))
            (list (- q 1) r)
            (let ((ret '()))
              (if (> q 0) (set! ret (baseX x q)))
              `(,@ret ,r))
            )))

    (list->string
     (map
      (lambda (d) (integer->char (+ A d)))
      (baseX 26 i)))
    ))

(define-public (glue-list lst glue)
  "create string from list containing arbitrary objects"
  (string-join (map (lambda (s) (format "~A" s)) lst) glue 'infix))
(define-public (glue-symbol lst . glue)
  "create symbol from list containig arbitrary objects"
  (string->symbol (string-join (map (lambda (s) (format "~A" s)) lst) (if (> (length glue) 0)(car glue) ":") 'infix)))

; custom string representation of a moment
(define-public (moment->string mom)
  "produce a human readable string from a moment"
  (if (ly:moment? mom)
      (let ((num (ly:moment-main-numerator mom))
            (den (ly:moment-main-denominator mom))
            (gnum (ly:moment-grace-numerator mom))
            (gden (ly:moment-grace-denominator mom)))
        (format "(~A/~A~A)" num den
          (cond
           ((> gnum 0)(format "+~A/~A" gnum gden))
           ((< gnum 0)(format "~A/~A" gnum gden))
           (else "")
           ))
        )
      "(?:?)"
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stack

; a stack implementation with methods stack-push, stack-pop and stack-get
(define-class <stack> ()
  (name #:accessor stack-get-name #:setter stack-set-name! #:init-value "stack")
  (store #:accessor stack-get-store #:setter stack-set-store! #:init-value '())
  )
(export stack-get-name)
(export stack-get-store)
(export stack-set-name!)
(export stack-set-store!)

(define-method (stack-push (stack <stack>) val)
  (set! (stack-get-store stack) (cons val (stack-get-store stack))))
(export stack-push)
(define-method (stack-get (stack <stack>))
  (let ((st (stack-get-store stack)))
    (if (> (length st) 0)
        (car st)
        #f)))
(export stack-get)
(define-method (stack-pop (stack <stack>))
  (let ((st (stack-get-store stack)))
    (if (> (length st) 0)
        (let ((ret (car st)))
          (set! (stack-get-store stack) (cdr st))
          ret)
        #f)))
(export stack-get)

(define-method (display (stack <stack>) port)
  (for-each
   (lambda (e)
     (format #t "~A> " (stack-get-name stack))(display e)(newline)) (stack-get-store stack)))

(define-public (stack-create)(make <stack>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tree

; a tree implementation
; every tree-node has a hashtable of children and a value
; main methods are:
; tree-set! <tree> path-list val: set a value in the tree
; tree-get <tree> path-list: get a value from the tree or #f if not present

(define-class <tree> ()
  (children #:accessor children #:init-thunk make-hash-table)
  (key #:accessor key #:init-keyword #:key #:init-value 'node)
  (value #:accessor value #:setter set-value! #:init-value #f)
  )

; set a value in a tree
(define-method (tree-set! (tree <tree>) (path <list>) val)
  (if (= (length path) 0)
      (set! (value tree) val)
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (not (is-a? child <tree>))
            (begin (set! child (make <tree> #:key ckey))
              (hash-set! (children tree) ckey child)
              ))
        (tree-set! child cpath val)
        ))
  val)
(export tree-set!)

(define-method (tree-merge! (tree <tree>) (path <list>) (proc <procedure>) val)
  (let ((ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>)
        (set! (value ctree) (proc (value ctree) val))
        (tree-set! tree path (proc #f val)))
    ))
(export tree-merge!)

(define-method (tree-get-tree (tree <tree>) (path <list>))
  (if (= (length path) 0)
      tree
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-tree child cpath)
            #f)
        )))
(export tree-get-tree)

(define-method (tree-get (tree <tree>) (path <list>))
  (let ((ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>) (value ctree) #f)))
(export tree-get)

(define-method (tree-get-from-path (tree <tree>) (path <list>) skey val)
  (if (equal? skey (key tree))(set! val (value tree)))
  (let ((child (hash-ref (children tree) skey)))
    (if (is-a? child <tree>)(set! val (value child))))
  (if (= (length path) 0)
      val
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-from-path child cpath skey val)
            val)
        )))
(export tree-get-from-path)

(define-method (tree-get-keys (tree <tree>) (path <list>))
  (if (= (length path) 0)
      (hash-map->list (lambda (key value) key) (children tree))
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-keys child cpath)
            #f)
        )))
(export tree-get-keys)

(define-method (tree-dispatch (tree <tree>) (path <list>) (relative <list>) def)
  (let ((val (value tree)))
    (if (= (length path) 0)
        (if val (cons '() val)(cons relative def))
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (or val (not (list? relative))) (set! relative '()))
          (if val (set! def (value tree)))
          (if (is-a? child <tree>)
              (tree-dispatch child cpath `(,@relative ,ckey) def)
              `((,@relative ,@path) . ,def))
          ))))
(export tree-dispatch)

(define-method (tree-collect (tree <tree>) (path <list>) (vals <stack>))
  (let ((val (value tree)))
    (if (> (length path) 0)
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (is-a? child <tree>) (tree-collect child cpath vals))
          ))
    (if val (push vals val))
    (reverse (store vals))
    ))
(export tree-collect)

(define (stdsort p1 p2)
  (let ((v1 (car p1))
        (v2 (car p2)))
    (cond
     ((and (number? v1) (number? v2)) (< v1 v2))
     ((and (ly:moment? v1) (ly:moment? v2)) (ly:moment<? v1 v2))
     (else (string-ci<? (format "~A" v1) (format "~A" v2)))
     )))
(define-method (tree-walk (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts)))
    (if (or doempty (value tree))
        (callback path (key tree) (value tree)))
    (for-each (lambda (p)
                (tree-walk (cdr p) `(,@path ,(car p)) callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
      (if dosort (sort (hash-table->alist (children tree)) sortby)
          (hash-table->alist (children tree)) ))
    ))
(export tree-walk)

(define-method (tree-walk-branch (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts))
        (ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>)
        (tree-walk ctree path callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
    ))
(export tree-walk-branch)

(define-public (tree-display tree . opt)
  (let ((path (ly:assoc-get 'path opt '() #f))
        (dosort (ly:assoc-get 'sort opt #t #f))
        (sortby (assoc-get 'sortby opt stdsort))
        (empty (ly:assoc-get 'empty opt #f #f))
        (dval (ly:assoc-get 'value opt #t #f))
        (vformat (ly:assoc-get 'vformat opt (lambda (v)(format "~A" v)) #f))
        (pformat (ly:assoc-get 'pformat opt (lambda (v)(format "~A" v)) #f))
        (pathsep (ly:assoc-get 'pathsep opt "/" #f))
        (port (ly:assoc-get 'port opt (current-output-port))))
    (tree-walk-branch tree path
      (lambda (path k val)
        (format #t "[~A] ~A" (key tree) (string-join (map pformat path) pathsep 'infix) port)
        (if (and dval val) (begin
                            (display ": " port)
                            (display (vformat val) port)
                            ))
        (newline port)
        ) `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,empty) )
    ))
(export tree-display)

(define-public (tree->string tree . opt)
  (call-with-output-string
   (lambda (port)
     (apply tree-display tree (assoc-set! opt 'port port))
     )))
(export tree->string)

(define-method (display (tree <tree>) port)
  (let ((tkey (key tree)))
    (tree-display tree)))

(define-public (tree? tree)(is-a? tree <tree>))
(define-public (tree-create . key)
  (let ((k (if (> (length key) 0)(car key) 'node)))
    (make <tree> #:key k)
    ))

