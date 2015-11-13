\version "2.18.2"

\include "openlilylib"

#(use-modules (_internal utilities tree))
#(use-modules (ice-9 format))

#(define (parser-lookup-symbol parser symbol)
	 (if (lilypond-greater-than? "2.19.21")
			 (ly:parser-lookup symbol)
			 (ly:parser-lookup parser symbol)))

%{!
% Create a new tree at the given symbol
%}
makeTree=
#(define-void-function
	 (parser location name) (symbol?)
	 (if (lilypond-greater-than? "2.19.21")
			 (ly:parser-define! name (make-tree))
			 (ly:parser-define! parser name (make-tree))))

%{!
% Set the value at the specified path. If the path already exists,
% update the old value
%}
treeSet=
#(define-void-function
	 (parser location name path value) (symbol? list? scheme?)
	 (tree-set! (parser-lookup-symbol parser name) path value))

%{!
% Get the value at the specified path. If the path exists, return
% the associated value, otherwise return the given default value.
%}
treeGet=
#(define-scheme-function
	 (parser location name path default) (symbol? list? scheme?)
	 (tree-get (parser-lookup-symbol parser name) path default))

%{!
% Get the value at the specified path. If the path exists, return
% the associated value, otherwise terminate with an error.
%}
treeGetStrict=
#(define-scheme-function
	 (parser location name path) (symbol? list?)
	 (catch 'key-error
					(lambda ()
						(tree-get (parser-lookup-symbol parser name) path))
					(lambda (key . args)
						(ly:error "~a: ~a" key (car args)))))

%{!
% Remove the value at the specified path. If the path does not exist,
% do nothing.
%}
treeRemove=
#(define-void-function
	 (parser location name path) (symbol? list?)
	 (tree-remove! (parser-lookup-symbol parser name) path))
