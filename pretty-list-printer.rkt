#lang racket

(define (print-lines list-of-strings)
  (cond ((not (null? list-of-strings))
         (display (car list-of-strings))
         (newline)
         (print-lines (cdr list-of-strings)))))

; will return of string of the char-string repeated however many times
(define (repeat-character char-string times)
  (if (= times 0)
    ""
    (string-append char-string (repeat-character char-string (- times 1)))))

; ╭───────┬
; │ front │
; ╰───────┴
; contents should be a string
(define (make-non-pointer-car contents)
  (let ((len (string-length contents)))
    (let ((first-line (string-append "╭" (repeat-character "─" (+ 2 len)) "┬"))
          (second-line (string-append "│ " contents " │"))
          (third-line (string-append "╰" (repeat-character "─" (+ 2 len)) "┴")))
      (list first-line second-line third-line))))
; (print-lines (make-non-pointer-car "front"))
; (newline)


; ──────╮
;  back │
; ──────╯
(define (make-non-pointer-cdr contents)
  (let ((len (string-length contents)))
    (let ((first-line (string-append (repeat-character "─" (+ 2 len)) "╮"))
          (second-line (string-append " " contents " │"))
          (third-line (string-append (repeat-character "─" (+ 2 len)) "╯")))
      (list first-line second-line third-line))))
; (print-lines (make-non-pointer-cdr "back"))
; (newline)

(define (make-pointer-car)
  (list "╭───┬" 
        "│ o │" 
        "╰─║─┴" 
        "  ║  " 
        "  V  "))

; not taking into consideration the long arrows!
(define (make-pointer-cdr)
  (list "───╮" 
        " o══" 
        "───╯"))

(define (longest-string-in-list list-of-strings)
  (define (iter list-of-strings best-so-far)
    (if (null? list-of-strings)
      best-so-far
      (iter (cdr list-of-strings)
            (let ((len (string-length (car list-of-strings))))
              (if (> len best-so-far)
                len
                best-so-far)))))
  (iter list-of-strings 0))
; this should give 7
; (longest-string-in-list (list "123456" "123" "1234567" "1234"))

(define (pad-string str len)
  (string-append str (repeat-character " " (- len (string-length str)))))

; add spaces to the end of all the strings so that they're all the
; same length
(define (pad-string-list list-of-strings)
  (let ((longest-length (longest-string-in-list list-of-strings)))
    (define (iter list-of-strings)
      (if (null? list-of-strings)
        '()
        (cons (pad-string (car list-of-strings) longest-length) (iter (cdr list-of-strings)))))
    (iter list-of-strings)))
; change the repeat character to something you can see
; (print-lines (pad-string-list (list "123456" "123" "1234567" "1234")))

(define (extend-cdr-pointer-arrow strings)
  (append (list (car strings))
          (list (string-append (cadr strings) 
                               (repeat-character 
                                 "═"
                                 (max (- (longest-string-in-list strings)
                                         (string-length (cadr strings)))
                                      1))
                               ">"))
          (list-tail strings 2)))

; this function assumes the left-string-list is padded to all be the same
; length
(define (join-string-lists left-string-list right-string-list)
  (let ((left-string-max-length (longest-string-in-list left-string-list)))
    (define (iter left-string-list right-string-list)
      (if (and (null? left-string-list) (null? right-string-list))
        '()
        (let ((left-side (if (null? left-string-list)
                           (cons "" null)
                           left-string-list))
              (right-side (if (null? right-string-list)
                            (cons "" null)
                            right-string-list)))
          (cons (string-append
                  (pad-string (car left-side)
                              left-string-max-length)
                  (car right-side))
                (iter (cdr left-side) (cdr right-side))))))
    (iter left-string-list right-string-list)))
; (print-lines (join-string-lists (list "1" "378") (list "2" "9" "3135" "1")))

; car-strings and cdr-strings are both list of strings, each
; element is a line of text
; cdr-strings will be a list of three strings that has to be appended
; to the car-strings
(define (glue-car-and-cdr car-strings cdr-strings)
  ; we know cdr-strings is a list of three strings, and it should
  ; be added to the first three strings of car-strings
  ; the car-strings will have a certain length, and the cdr-strings
  ; will have a certain length
  (append (list (string-append (car car-strings) (car cdr-strings)))
          (list (string-append (cadr car-strings) (cadr cdr-strings)))
          (list (string-append (caddr car-strings) (caddr cdr-strings)))
          (list-tail car-strings 3)))
; (print-lines (glue-car-and-cdr (make-pointer-car) (make-pointer-cdr)))  


(define (turn-into-string x)
  (cond ((string? x) (string-append "\"" x "\""))
        ((symbol? x) (string-append "'" (symbol->string x)))
        ((null? x) "/")
        ((number? x) (number->string x))))

; this procedure returns a list of strings, which is a picture of
; everything refered to by the-cons
; so it's going to be recursive
; we're going to assume the-cons is actually a cons
(define (print-cons the-cons)
  ; first we gotta print the car of the cons
  (let ((car-strings 
          (if (pair? (car the-cons))
            (append (make-pointer-car) (print-cons (car the-cons)))
            (make-non-pointer-car (turn-into-string (car the-cons))))))
    (if (not (pair? (cdr the-cons)))
      (pad-string-list (glue-car-and-cdr car-strings (make-non-pointer-cdr
                                                       (turn-into-string (cdr the-cons)))))
      (join-string-lists (extend-cdr-pointer-arrow 
                           (glue-car-and-cdr car-strings (make-pointer-cdr)))
                         (print-cons (cdr the-cons))))))

; (print-lines (print-cons (cons (cons 3 4) (cons 'meow 'bitch))))
; (print-lines (print-cons (cons 3 4)))
; (print-lines (print-cons (cons 'hek (cons 'meo '()))))
; (print-lines (print-cons (cons (cons 3 4) '())))
; (print-lines (print-cons (cons (cons 3 (cons 4 '())) "hello")))
(print-lines (print-cons (cons (cons 3 (cons 4 '())) (cons (cons 4 (cons 'meow '())) "hello"))))
