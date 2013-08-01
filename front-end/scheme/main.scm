#lang racket
;(*
; * =====================================================================================
; *
; *       Filename:  main.scm
; *
; *    Description:  The top level of Ruse scheme front-end
; *
; *         Author:  Adriaan Larmuseau, ajhl
; *        Company:  Distrinet, Kuleuven
; *
; * =====================================================================================
; *)

(require "byte.scm")
(require "normalize.scm")
(require "helper.scm")

; read program as list
(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-all)))))

; START POINT
(define the-program (read-all))  ; read expr., pass to eval, write result
(define normalized (normalize-program the-program))
(debug "The Program :: ")
(debug the-program)
(debug "\n\n")
(debug normalized) 
(create-result emit normalized)


