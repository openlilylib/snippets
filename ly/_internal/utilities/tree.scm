;; Tree module
;; ===========
;;
;; This module implements a tree data structure that allows to
;; associate values with paths. Values can be arbitrary scheme
;; entities, whereas paths are represented as lists.
;;
;; Public API overview
;; -------------------
;;
;; First of all, a word on the naming convention adopted. All the
;; public functions that modify the tree have their name followed by
;; an exclamation mark (for instance tree-set! and
;; tree-remove!). Functions whose name is not followed by an
;; exclamation mark do not modify the tree (like all lookup functions,
;; e.g. tree-get and tree-lookup).
;;
;; ### Tree creation
;;
;; To create a new tree, use the function (make-tree).
;;
;; ### Insertion of values
;;
;; Once you have a tree object, you can start to associate values to
;; paths witht the function
;;
;;     (tree-set! tree path value)
;;
;; ### Removal of subtrees
;;
;; You can remove entire subtrees (and all their associated values)
;; with the function
;;
;;     (tree-remove! tree path)
;;
;; that, if the specified path does not exists in the tree, simply
;; does nothing.
;;
;; ### Retrieval of values
;;
;; To retrieve values from the tree, you have several options
;;
;; 1. The function `tree-get'. You can use this function as follows
;;
;;        (tree-get tree path default)
;;
;;    If the requested path is in the tree, the associated value is
;;    returned, otherwise the function returns the default value,
;;    which is an optional argument. If the default value is not
;;    specified and the requested path does not exist in the tree,
;;    then a 'key-error exception is thrown. See
;;    https://www.gnu.org/software/guile/docs/docs-1.8/guile-ref/Catch.html
;;    for information on how to handle exceptions.
;;
;; 2. The function `tree-get-node', that is used as
;;
;;        (tree-get-node tree path)
;;
;;    This function returns the node associated with the given path if
;;    it exists, and #f otherwise. You can use it in combination with
;;    the `node-value' function to extract the value stored in the
;;    nodes, in case of success, as follows
;;
;;        (let ((node (tree-get-node tree path)))
;;					(if node
;;							(let ((value (node-value node)))
;;								;; Do something with value
;;								)
;;							(begin
;;								;; Do something to handle the missing value
;;								)))        
;;
;; 3. The function `tree-lookup'. This function makes the use case
;;    described in the last example simpler to use. You invoke it as
;;    follows
;;
;;        (tree-lookup tree path success-fn failure-fn)
;;
;;    where `success-fn' is a function to call if the path exists in
;;    the tree, and `failure-fn' is a function to call in case the
;;    path does not exists. So, for instance, you have
;;
;;        (tree-lookup tree path
;;				  (lambda (value)
;;						;; do something with value
;;						)
;;					(lambda (missing-path)
;;						;; do something to handle the missing path
;;						))
;;
;; ### Clearing a tree
;;
;; If you need to reset a given tree to its empy state, just call
;;
;;     (tree-clear! tree)
;;
;; Rationale
;; ---------
;;
;; This scheme module is heavily based on the the `alist-access.ily'
;; module by Jan-Peter Voigt. It tries to address some concerns with
;; the previous approach (see, for instance,
;; https://github.com/openlilylib/openlilylib/issues/68), namely
;;
;; 1. The function to get values from the tree has a not well defined
;;    behaviour for missing values: in case of missing values it will
;;    return #f, however #f is also a legitimate value, hence we
;;    cannot distinguish in wuich case we are into.
;;
;; 2. All the tree-related functions accept a parser and a location as
;;    arguments. This means that they can be used only in contexts
;;    where such values are in scope. This means that these values
;;    need to be passed down the call stack, leading to code that is,
;;    arguably, more cluttered.
;;
;; In order to address these issues, this module defines a
;; well-specified public interface, where missing values are signaled
;; without ambiguity (see the API averview above for a
;; summary). Moreover, none of this module's functions relies on
;; `parser' and `location' values, hence they are simpler to use in
;; contexts where such values are unavailable.
;;
(define-module (_internal utilities tree)
	#:export (make-tree
						tree-set!
						tree-remove!
						tree-get
						tree-get-node
						node-value
						tree-lookup
						tree-clear!)
	#:use-module ((oop goops)
								(ice-9 format)))

;;
;; Node
;; ====
;;
;; Node objects are private to this module. The only public function
;; accessible from other modules is `node-value', that returns the
;; value associated to a given node.
;;
;; NOTE: <node> and <tree> make use of the object-oriented features of
;; Guile scheme. For more information on the topic, have a look to
;; http://www.gnu.org/software/guile/manual/html_node/GOOPS.html
;;
;; A node is an object with four fields, namely
;;
;;  - name: is used to determine if a node is part of a path
;;
;;  - has-value: is used to establish if the value field contains a
;;               valid value. This allows to distinguish between nodes
;;               whose value is '() because they have no values from
;;               nodes whose value is actually '().
;;  - value: the value stored in the node. Note that even internal
;;           nodes of the tree can contain a value.
;;  - children: a list of children of this node.
;;
(define-class <node> ()
	(name #:init-keyword #:name
				#:getter name)
	(has-value #:init-value #f
						 #:init-keyword #:has-value
						 #:accessor has-value)
	(value #:init-value '()
				 #:init-keyword #:value
				 #:accessor value)
	(children #:init-value '()
						#:accessor children))

;; Given a node, return its associated value. If the node has no
;; associated value (i.e. (= #f (has-value node))), then a
;; 'missing-value exception is thrown.
(define-method (node-value (n <node>))
	(if (has-value n)
			(value n)
			(throw 'missing-value
						 "The node exists, but has no associated value")))

;; Adds a single child to the given node
(define-method (add-child (n <node>) (c <node>))
	(let ((new-children (cons c (children n))))
		(set! (children n) new-children)
		n))

;; Adds a list of children to the given node
(define-method (add-children (n <node>) children)
	(if (null? children)
			n
			(let ((new-node (add-child n (car children))))
				(add-children new-node (cdr children)))))

;; Removes a single child by name. If the child does not exist, then
;; this function does nothing.
(define-method (remove-child (n <node>) child-name)
	(let ((new-children (filter (lambda (child)
																(not (equal? (name child) child-name)))
											 (children n))))
		(set! (children n) new-children)
		n))

;; Gets a single child by name. If the child is not present, then
;; return #f. Note that there is no ambiguity when this function
;; returns #f. A node whose value is #f is a different entity with
;; respect to #f itself.
(define-method (get-child (n <node>) child-name)
	(let ((matching-children (filter (lambda (child)
																		 (equal? (name child) child-name))
																	 (children n))))
		(if (null? matching-children)
				#f
				(car matching-children))))

;; Overrides the `display' function for nodes. 
(define-method (display (n <node>) port)
	(format port "<~a . ~a ~a>" (name n) (has-value n) (value n)))

;;
;; Tree 
;; ====
;;
;; A tree is simply an object with a root node. This root node is
;; names "root", has no value and initially it does not have any
;; child.
;;
(define-class <tree> ()
	(root #:init-form (make <node> #:name "root")
				#:accessor root))

;; Create a new empty tree
(define (make-tree) (make <tree>))

;; Utility method. Associates the given value to the given path, as a
;; subtree of node `n'.
(define-method (subtree-set! (n <node>) path val)
	(cond
	 ((null? path) (error "Path in a tree cannot be empty"))
	 ((null? (cdr path)) (let ((child (get-child n (car path))))
												 (if child
														 (begin
															 (set! (value child) val)
															 (set! (has-value child) #t))
														 (let ((c (make <node>
																				#:name (car path)
																				#:value val
																				#:has-value #t)))
															 (add-child n c)))))
	 (else (let ((child (get-child n (car path))))
					 (if child
							 (subtree-set! child (cdr path) val)
							 (let ((c (make <node> #:name (car path))))
								 (add-child n c)
								 (subtree-set! c (cdr path) val)))))))

;; Associate the given value to the given path. If the path already
;; exists, then the value is update, otherwise any missing node is
;; created.
(define-method (tree-set! (t <tree>) path val)
	(subtree-set! (root t) path val))

;; Utility function. Retrieves the node associated with `path',
;; starting from the given node.
(define-method (subtree-get-node (n <node>) path)
	(cond
	 ((null? path) (error "Path in a tree cannot be empty"))
	 ((null? (cdr path)) (get-child n (car path)))
	 (else (let ((c (get-child n (car path))))
					 (if c
							 (subtree-get-node c (cdr path))
							 #f)))))

;; Retrieves the node associated with `path'. If `path' does not
;; exist, then return #f
(define-method (tree-get-node (t <tree>) path)
	(subtree-get-node (root t) path))

;; Look up `path' in tree `t'. If the path exists, return its
;; associated value. Otherwise, if a `default-value' is give return
;; it, else throw a 'key-error exception.
(define (tree-get t path . default-value)
	(let ((n (tree-get-node t path)))
		(cond
		 (n (node-value n))
		 ((not (null? default-value)) (car default-value))
		 (else (throw 'key-error
									(format #f "Path ~a not present in tree" path))))))

;; Lookup a path in the tree. If the path exists, then invoke
;; success-fn with the value as argument, otherwise invoke failure-fn
;; with the path as argument.
(define-method (tree-lookup (t <tree>) path success-fn failure-fn)
	(let ((node (tree-get-node t path)))
		(if node
				(success-fn (node-value node))
				(failure-fn path))))

;; Utility function. Remove a path starting from the given node. If
;; the path dows not exist, then do nothing. Note that if the path
;; does not end at a leaf node, then an entire subtree is removed.
(define-method (subtree-remove! (n <node>) path)
	(cond
	 ((null? path) (error "Path in a tree cannot be empty"))
	 ((null? (cdr path)) (remove-child n (car path)))
	 (else (let ((c (get-child n (car path))))
					 (if c
							 (subtree-remove! c (cdr path))
							 #f)))))

;; Remove a path from the tree. If the path does not exist, then
;; nothing happens. Note that if the path does not end at a leaf node,
;; then an entire subtree is removed.
(define-method (tree-remove! (t <tree>) path)
	(subtree-remove! (root t) path))

;; Remove all the nodes from the tree.
(define-method (tree-clear! (t <tree>))
	(set! (children (root t)) '()))

;; Utility function: recursively display a subtree.
(define (rec-display port n indent)
	(format port "~a~a\n" indent n)
	(for-each
	 (lambda (c)
		 (rec-display port c (string-append indent "    ")))
	 (children n)))

;; Overrides `display' for tree. Pretty prints the tree on the given
;; port.
(define-method (display (t <tree>) port)
	(rec-display port (root t) "")
	(newline port))
