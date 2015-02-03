
% Global variable holding an absolute path to openlilylib's root directory
#(define openlilylib-root "")


setRootPath =
#(define-void-function (parser location)()
   (let* ((path (get-normalized-path (ly:input-file-line-char-column location))))
     (set! openlilylib-root path)
     (if (not (member path %load-path))
         (set! %load-path `(,path ,@%load-path)))))

