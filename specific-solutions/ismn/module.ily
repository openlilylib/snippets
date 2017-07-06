\version "2.17.97"
% should work in earlier versions

\header {
  snippet-title = "Calculate ISMN checksum"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    \wordwrap {
      Calculate the checksum for an ISM-Number with a given publisher and title number and return a string
      \concat { "\"M " \italic <pppp> "-" \italic <tttt> "-" \italic <c> }.
    }
  }
  tags = "ismn, scheme"
  status = "ready"
}

% create ISMN string with publisher number and title number
#(define-public (create-ismn publisher title)
   "produce a string 'M <pppp> <tttt> <c>' where pppp is the publisher number, tttt the title number and c the checksum."
  (let ((ismn-list (string->list "9790"))
        (check 0))
    ; append character lists from publisher and title to ISMN key "9790"
    (set! ismn-list (append ismn-list (string->list publisher)))
    (set! ismn-list (append ismn-list (string->list title)))
    ; calculate checksum as described in http://en.wikipedia.org/wiki/International_Article_Number_%28EAN%29
    (let ((p 0)(i 0))
      (for-each (lambda (c)(let ((n (- (char->integer c)(char->integer #\0))))
                             (set! i (1+ i))
                             (if (= 0 (modulo i 2))(set! n (* 3 n)))
                             (set! p (+ p n))
                             )) ismn-list)
      (set! check (modulo (- 10 (modulo p 10)) 10) )
      )
    ; create string
    (string-append "M " publisher "-" title "-" (number->string check))
    ))

% wrap scheme-function
createISMN = #(define-scheme-function (parser location publisher title)(string? string?)(create-ismn publisher title))

% markup command
#(define-markup-command (ismn layout props publisher title)(string? string?)   
   (interpret-markup layout props (create-ismn publisher title)))

