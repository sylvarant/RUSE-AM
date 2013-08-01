#lang racket
;(*
; * =====================================================================================
; *
; *       Filename:  helper.scm
; *
; *    Description:  Helper functionality shared across the front-end
; *
; *         Author:  Adriaan Larmuseau, ajhl
; *        Company:  Distrinet, Kuleuven
; *
; * =====================================================================================
; *)

(provide error debug emit produce_ls prim?)

;;; create binding for error
(define error #f)

;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
              (set! error
                (lambda error-arguments
                  (display ">>>> ERROR ")
                  (newline)
                  (k error-arguments)))

)) 

; Output debug information
(define (debug str)
    (with-output-to-file "/dev/stderr"
            (lambda ()
                (display str)) #:exists 'append ))


;; Print helper
(define (emit line)
  (display line)
  (newline)
)

;; Only needed once λ is typed
(define (produce_ls a b)
    (if (a > 0) 
        (cons b (produce_ls (- a 1) b))    
        '()
))


; prim? : symbol? -> boolean?
(define (prim? exp)
  (case exp
    [(+ - * =) #t]
    [else      #f]))
