;-----------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------ protocol -------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------

#lang racket
(require racket/gui/base)
(require "basic.rkt")
(#%provide usage? switch? make-protocol-boxes calc-protocol empty-protocol-boxes)
;protocol is een getal gemaakt uit priemgetallen
;2 -> heeft electronische gebruik meter
;3 -> heeft een aan en uit knop

;check of een zekere waarde x deelbaar is door een zekere waarde
(define (protocol? x check)
  (if (zero? (modulo x check))
      true
      false))

(define (usage? x)
  (protocol? x 2))
       
(define (switch? x)
  (protocol? x 3))

;om het toevoegen van extra protocollen op 1 plaats te houden staat dit hier, hoort zowel bij gui als bij protocol
(define (make-protocol-boxes parent)
  (define usage-meter
    (new check-box% 
       [parent parent]
       [label "usage-meter?"]
       ))
  (define switch
  (new check-box%
       [parent parent]
       [label "switch?"]
       ))
  (conjoin usage-meter switch))

(define (calc-protocol protocol-boxes)
  (* (if (send (first protocol-boxes) get-value) 2 1)
     (if (send (next protocol-boxes) get-value) 3 1)
     ))

(define (empty-protocol-boxes protocol-boxes)
  (send (first protocol-boxes) set-value false)
  (send (next protocol-boxes) set-value false))