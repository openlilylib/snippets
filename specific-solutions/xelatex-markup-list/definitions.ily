\version "2.18.0"

\header {
  snippet-title = "xelatex-markup-list command"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    \wordwrap {
      Include xelatex source to create markup-list with one EPS-stencil per page.
    }
  }
  tags = "markup,latex,xelatex"
  status = "unknown"
}

#(use-modules
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 regex)
 (scm framework-eps)
)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ... utilities ...

% this predicate is not public?
#(define (markup-function? x)
  (and (markup-command-signature x)
       (not (object-property x 'markup-list-command))))

% calculate available page-height
#(define-public (content-height layout props)
  (let ((mm (ly:output-def-lookup layout 'mm))
        (height (ly:output-def-lookup layout 'paper-height))
        (perc (chain-assoc-get 'percent props 100))
        (tm (ly:output-def-lookup layout 'top-margin))
        (bm (ly:output-def-lookup layout 'bottom-margin))
        (hom (ly:output-def-lookup layout 'oddHeaderMarkup))
        (hem (ly:output-def-lookup layout 'evenHeaderMarkup))
        (bom (ly:output-def-lookup layout 'oddFooterMarkup))
        (bem (ly:output-def-lookup layout 'evenFooterMarkup)))
    (define (mupheight mup)(if (markup? mup) (let ((y-ext (ly:stencil-extent (interpret-markup layout props mup) Y)))
                                               (- (cdr y-ext)(car y-ext))) 0))
    (/ (* (- height (+ tm bm (max (mupheight hom)(mupheight hem))
                      (max (mupheight hom)(mupheight hem)))) (/ perc 100)) mm)
    ))

% join string on arbitrary objects
#(define-public (glue-list lst glue)
  "create string from list containing arbitrary objects"
  (string-join (map (lambda (s) (format "~A" s)) lst) glue 'infix))

% read text from multiline comment %{ %}
#(define-public (read-comment port linenr)
   (let ((rstart (make-regexp "^[^%]*%\\{(.*)$"))
         (rend (make-regexp "^(.*)%}.*$")))
     (define (collect lc status . lines)
       (let ((line (read-line port 'concat)))
         (if (string? line)
             (cond
              ((< lc linenr)
               (apply collect (+ lc 1) 0 lines))
              ((= status 0)
               (let ((match (regexp-exec rstart line)))
                 (if (regexp-match? match)
                     (let ((i (match:start match 1)))
                       (apply collect (+ lc 1) 1 (append lines (list (substring line i))))
                       )
                     (apply collect (+ lc 1) 0 lines)
                     )))
              ((= status 1)
               (let ((match (regexp-exec rend line)))
                 (if (regexp-match? match)
                     (let ((i (match:start match 1)))
                       (apply collect (+ lc 1) 2 (append lines (list (match:substring match 1))))
                       )
                     (apply collect (+ lc 1) 1 (append lines (list line)))
                     )))
              (else (apply string-append lines))
              )
             (apply string-append lines))
         ))
     (collect 1 0)
     ))

% scheme function to read comment: \readComment
readComment =
#(define-scheme-function (parser location)()
   (let* ((fll (ly:input-file-line-char-column location))
          (file (car fll))
          (linenr (cadr fll))
          (port (open-file file "r")))
     (read-comment port linenr)
     ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% clean environment from lilypond entries

ENV_PATH_SEPARATOR = ":"
%FILTER_ENV = #'("PATH" "LD_LIBRARY_PATH" "INSTALLER_PREFIX" "FONTCONFIG_FILE" "FONTCONFIG_PATH" "PANGO_RC_FILE" "PANGO_PREFIX" "PANGO_MODULE" "GUILE_LOAD_PATH" "GS_LIB")
FILTER_ENV = #'("PATH" "LD_LIBRARY_PATH")

#(define-public (filter-path-string str re)
   (string-join
    (filter-path-list (string-split str (string-ref ENV_PATH_SEPARATOR 0)) re)
    ENV_PATH_SEPARATOR 'infix))
#(define-public (filter-path-list pl re)
   (filter
    (lambda (s) (not (regexp-exec re s)))
    pl))

#(define-public (environ-clean)
   (let* ((lyre (make-regexp "^.*lilypond.*$" regexp/icase))
          (envl (environ))
          (enva (map (lambda (s)
                       (let ((i (string-index s #\=)))
                         (if (integer? i)
                             (cons (substring s 0 i) (substring s (+ 1 i)))
                             (cons s "TRUE")
                             ))) envl)))
     (set! enva
           (map
            (lambda (p)
              (let ((keye (car p))
                    (vale (cdr p)))
                (if (member keye FILTER_ENV)
                    (set! vale (filter-path-string vale lyre)))
                (cons keye vale)
                )) enva))
     ;(map (lambda (p) (format "~A=\"~A\"" (car p)(cdr p))) (filter (lambda (p) (> (string-length (cdr p)) 0)) enva))
     (map (lambda (p) (format "~A=\"~A\"" (car p)(cdr p))) enva)
     ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ... call tex ...

% produce tex source from markup    
#(define-public (markup->tex mup . mprops)
  (let ((conc (if (> (length mprops) 0) (car mprops) #f))
        (layout (if (> (length mprops) 1) (cadr mprops) #f))
        (props (if (> (length mprops) 2) (caddr mprops) '()))
        (mup? (lambda (m)(or (string? m)(list? m)(markup? m))))
        (result ""))
    (cond ((string? mup) (set! result mup))
      ((null? mup) (set! result ""))

      ((and (pair? mup) (equal? (car mup) concat-markup))
       (set! result (markup->tex (cdr mup) #t layout props)))

      ((and (pair? mup) (equal? (car mup) fromproperty-markup))
       (set! result (markup->tex (chain-assoc-get (cadr mup) props "???") conc layout props)))

      ((and (pair? mup) (equal? (car mup) override-markup))
       (set! result (markup->tex (cdr (cdr mup)) conc layout (cons (list (car (cdr mup))) props))))
      ((and (pair? mup) (equal? (car mup) page-ref-markup))
       (set! result (let* ((table (if layout (ly:output-def-lookup layout 'label-page-table) '()))
                           (pg (assoc-get (car (cdr mup)) table)))
                      (if pg (format "~A" pg) (caddr (cdr mup)))) ))

      ((and (pair? mup)(markup-function? (car mup)))
       (let ((proc (get-markup-producer (car mup))))
         (if (procedure? proc)
             (set! result (markup->tex (proc layout props (cdr mup))))
             (for-each (lambda (m)(set! result (string-append result (if (or conc (string=? result "")) "" " ") (markup->tex m conc layout props))))
               (filter mup? (cdr mup))))))

      ((list? mup)
       (for-each (lambda (m)(set! result (string-append result (if (or conc (string=? result "")) "" " ") (markup->tex m conc layout props))))
         (filter mup? mup)))
      (else (ly:message "~A" mup)))
    result))

% create eps-markup-list from pdf
#(define-public (tex-markup-list layout props pkgs cmd opts m)
  (let* ((mm (ly:output-def-lookup layout 'mm))
         (padstart (chain-assoc-get 'padstart props 2))
         (padlength (* mm (chain-assoc-get 'padlength props 0)))
         (scropts (chain-assoc-get 'scrartcl props ""))
         (size (chain-assoc-get 'line-width props (ly:output-def-lookup layout 'line-width 10)))
         ; width of our box in mm
         (width (let ((tw (chain-assoc-get 'tex-width props #f))) (if tw (begin (set! size (* tw mm)) tw) (/ size mm))))
         ; percent of page to use
         (perc (chain-assoc-get 'percent props))
         ; height of our box in mm
         (height (chain-assoc-get 'tex-height props (- (content-height layout props) (chain-assoc-get 'bottom-gap props (if perc 0 3)))))
         ; the text to fill into template.tex
         (text (if (and (string? m) (file-exists? m)) (ly:gulp-file m) (markup->tex m #f #f props)))
         ; basename of working files
         (basename (strftime (format "~A-%Y%m%d%H%M%S" cmd) (localtime (current-time))))
         ; result of each command
         (result 0)
         ; stencil to return
         (text-stencil empty-stencil)
         (pages 0)
         (epslist '()))
    (define (readpipe port text)
      (let ((line (read-line port)))
        (if (not (eof-object? line))
            (set! text (readpipe port (string-append (string-append text line) "\n"))))
        text))
    (define (cmd->string cmd)
      (let* ((port (open-pipe cmd OPEN_READ))
             (str (readpipe port ""))
             (result (close-pipe port)))
        (if (and (= 0 result)(> (string-length str) 0))
            str "")))
    (define (loop n callback)(let ((ret (list)))
                               (define (lp i)
                                 (if (<= i n)
                                     (begin
                                      (set! ret (append ret (list (callback i))))
                                      (lp (+ i 1)))))
                               (lp 1)
                               ret))

    ; write <basename>.tex
    (let ((tex-src
           (format "\\documentclass~A{scrartcl}
\\usepackage[paperwidth=~Amm,paperheight=~Amm,margin=~Amm]{geometry}
\\usepackage[~A]{babel}
~A
\\begin{document}
~A
\\end{document}
"
scropts width height
(chain-assoc-get 'tex-margin props 1)
(chain-assoc-get 'babel props "ngerman")
(glue-list pkgs "\n")
text)))
      (with-output-to-file (format "~A.tex" basename) (lambda () (display tex-src)))
      )
    ; produce pdf
    (with-output-to-file (format "~A.sh" basename)
      (lambda ()
        (for-each (lambda (s) (display s)(newline)) (environ-clean))
        (format #t "~A ~A \"~A.tex\"" cmd opts basename)(newline)
        (format #t "N=`pdfinfo \"~A.pdf\" | sed -n -e 's/^Pages: *\\([0-9]*\\).*$/\\1/gp'`" basename)(newline)
        (format #t "echo -ne $N >\"~A.pages\"" basename)(newline)
        (format #t "for (( p=1; p<=$N; p++ )) ; do pdftops -eps -f $p -l $p \"~A.pdf\" \"~A.eps\" ; done" basename basename)(newline)
    ))
    (set! result (system (format "/bin/bash \"~A.sh\"" basename)))
    ; how many pages
    (set! pages (string->number (ly:gulp-file (format "~A.pages" basename))))
    ; add pages to markup-list
    (loop pages (lambda (pag)(let ((pagname (format "~A-~A.eps" basename pag)))
                               ; convert page to EPS
                               (set! result (system (format "pdftops -eps -f ~A -l ~A \"~A.pdf\" \"~A\"" pag pag basename pagname)))
                               ; include EPS
                               (set! text-stencil (eps-file->stencil X size pagname))
                               (if (and (>= pag padstart)(> padlength 0))
                                   (set! text-stencil (ly:stencil-combine-at-edge
                                                       (ly:make-stencil '() (cons 0 size) (cons 0 padlength))
                                                       Y -1 text-stencil 0)))
                               (set! epslist (append epslist (list text-stencil)))
                               )))

    (system (format "cat \"~A.sh\"" basename))
    ; remove working files
    (system (format "rm -v \"~A\"*" basename))
    ; return eps-stencil
    epslist
    ))

% pdflatex markup-list command
#(define-markup-list-command (pdflatex layout props m)(markup-list?)
  (tex-markup-list layout props
    `("\\usepackage[utf8]{inputenc}") "pdflatex" "-interaction=batchmode" m))
% pdflatex markup-list include command
#(define-markup-list-command (pdflatexInclude layout props m)(string?)
  (tex-markup-list layout props
    `("\\usepackage[utf8]{inputenc}") "pdflatex" "-interaction=batchmode" m))

% xelatex markup-list command
#(define-markup-list-command (xelatex layout props m)(markup-list?)
  (let ((font-name (chain-assoc-get 'font-name props #f)))
    (if (not (string? font-name))
        (begin
         (set! font-name (chain-assoc-get 'font-family props #f))
         (if (string? font-name)
             (ly:warning "using deprecated property 'font-family for '~A'" font-name)
             (begin
              (set! font-name (get-registry-val lalily:latex:default-font "DejaVu Serif"))
              (ly:warning "no font-name defined! trying '~A' ..." font-name)
              ))
         ))
    (tex-markup-list layout props
      `("\\usepackage[T1]{fontenc}" "\\usepackage{fontspec}"
         "\\defaultfontfeatures{Mapping=tex-text}"
         ,(format "\\setmainfont{~A}" font-name)
         ,@(chain-assoc-get 'packages props '())
         )
      "xelatex" "-interaction=batchmode" m)
    ))

% xelatex markup-list include command
#(define-markup-list-command (xelatexInclude layout props m)(string?)
  (let ((font-name (chain-assoc-get 'font-name props #f)))
    (if (not (string? font-name))
        (begin
         (set! font-name (chain-assoc-get 'font-family props #f))
         (if (string? font-name)
             (ly:warning "using deprecated property 'font-family for '~A'" font-name)
             (begin
              (set! font-name "DejaVu Serif")
              (ly:warning "no font-name defined! trying '~A' ..." font-name)
              ))
         ))
    (tex-markup-list layout props
      `("\\usepackage[T1]{fontenc}" "\\usepackage{fontspec}"
         "\\defaultfontfeatures{Mapping=tex-text}"
         ,(format "\\setmainfont{~A}" font-name)
         ,@(chain-assoc-get 'packages props '())
         )
      "xelatex" "-interaction=batchmode" m)))

