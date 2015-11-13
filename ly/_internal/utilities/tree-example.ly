\version "2.18.2"

\include "tree.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Scheme example
%%

#(begin
	 (display "Scheme example\n==============\n\n")
	 
	 (format #t "0. Create an empty tree\n")
	 (define t (make-tree))
	 (format #t "\n~a\n" t)

	 (format #t "1. Add some content\n")

	 (tree-set! t '("a" "b" "c") 10)
	 (tree-set! t '("a" "b") 5)
	 (tree-set! t '("a" "d" "ce") 14)

	 (format #t "\n~a\n" t)

	 (format #t "2. Get value for key '(a b c)\n\n~a\n\n"
					 (tree-get t '("a" "b" "c")))

	 (format #t "3. Get value for non-existing key with default '(a b d)\n\n~a\n\n"
					 (tree-get t '("a" "b" "d") "default value"))

	 (format #t "4. Get node for key '(a b c)\n\n")

	 (define n (tree-get-node t '("a" "b" "c")))
	 (format #t "node is ~a, value is ~a\n\n"
					 n (node-value n))

	 (format #t "5. Remove subtree rooted at '(a d)\n")
	 (tree-remove! t '("a" "d"))
	 (format #t "\n~a\n" t)

	 (format #t "6. Try to access a non-existing key, without default\n\n")
	 (catch 'key-error
					(lambda ()
						(tree-get t '("a" "d")))
					(lambda (key . args)
						(format #t "Caught exception ~a: ~a\n\n"
										key (car args))))

	 (format #t "7. Try to access an internal node that has no value\n\n")

	 (catch 'missing-value
					(lambda ()
						(format #t "The value is ~a\n\n"
										(tree-get t '("a"))))
					(lambda (key . args)
						(format #t "Caught exception ~a: ~a\n\n"
										key (car args))))

	 (format #t "8. Use the tree-lookup function for existing key\n\n")

	 (tree-lookup t '("a" "b" "c")
								(lambda (value)
									(format #t "Found value: ~a\n\n" value))
								(lambda (missing)
									(format #t "Key ~a not found, do something\n\n" missing)))

	 (format #t "9. Use the tree-lookup function for non existing key\n\n")

	 (tree-lookup t '("non existent key")
								(lambda (value)
									(format #t "Found value: ~a\n\n" value))
								(lambda (missing)
									(format #t "Key ~a not found, do something\n\n" missing)))

	 (format #t "10. Clear the tree\n")
	 (tree-clear! t)
	 (format #t "\n~a\n" t))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lilypond example
%%

#(display "Lilypond example\n================\n\n")

\makeTree test

\treeSet test #'(a b c) "test value"

\treeGet test #'(a b c) "default value"
\treeGetStrict test #'(a b c)

#(display test)

\treeRemove test #'(a b)

#(format #t "~a\n\n" test)

%% If you uncomment this, you will get an error message because you
%% are looking for a non-existing path

%\treeGetStrict test #'(a b d)
