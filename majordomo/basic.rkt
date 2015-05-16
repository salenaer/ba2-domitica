;-----------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------ basic ----------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------

#lang racket
(require compatibility/mlist)
(#%provide (all-defined))


(define (send-message target message . args)
  (apply (target message) args))

;basis abstraties
(define empty-list '())
(define none-left? empty?)
(define conjoin cons)
(define first car)
(define next cdr)
(define my-loop map)
(define my-loop-m mmap)
(define my-flatten append*)
(define size length)
(define take-newest take-right)
(define vector->mylist vector->list)

(define (element-of? el lst)
  (cond ((none-left? lst) #f)
        ((equal? (first lst) el) #t)
        (else (element-of? el (next lst)))))

(define (erase-by-copy lst el comp-function)
  (define (loop build resolt)
    (cond ((none-left? build) resolt)
          ((comp-function build el) (loop (next build) resolt))
          (else (loop (next build)(conjoin (first build) resolt)))))
  (loop lst empty-list))


;voor makkelijk gebruik poorten
(define (make-port-pair input output)
  (cons input output))

(define (get-input port-pair)
  (car port-pair))

(define (get-output port-pair)
  (cdr port-pair))

(define (my-write message port-pair)
  (write message (get-output port-pair))
  (flush-output (get-output port-pair)))

(define (my-read port-pair)
  (read (get-input port-pair)))

(define (my-flush port-pair)
  (flush-output (get-output port-pair)))

;abstractie voor data pakketten
(define (make-data-package device data)
  (cons device data))

(define (get-device data-package)
  (car data-package))

(define (get-data data-package)
  (cdr data-package))

;voor de database
(define (data-table-name steward device)
  (string-append steward device))

(define (list-min lst)
  (if (none-left? lst)
      (error "list is empty, list-min")
      (let ((smallest (first lst)))
        (define (loop lst)
          (if (none-left? lst)
              smallest
              (begin 
                (when (< (first lst) smallest)
                  (set! smallest (first lst)))
                (loop (next lst)))))
        (loop (next lst)))))

;maak al mijn lijsten even lang
(define (shrink lol)
  (let ((short (list-min (my-loop size lol)))
        (resoltlists empty-list))
    (define (loop lol)
      (unless (none-left? lol)
        (set! resoltlists (conjoin (take-newest (first lol) short) resoltlists)) ;voeg het rechter deel van de lijst toe aan de resultaten lijst
        (loop (next lol))))
    (loop lol)
    resoltlists))

(define (average-lists lol)
  (if (none-left? lol)
      (error "no elements in list average-lists")
      (let ((resoltlists (shrink lol))
            (listcount (size lol)))
        ;bereken gemiddelde van alle lijsten
        (define (average lol)
          ;tel de eerste waarden van alle lijsten op en deel door het aantal lijsten
          (define (count lst)
            (let ((resolt 0))
              (my-loop (lambda (x)(set! resolt (+ resolt x))) lst)
              (/ resolt listcount)))
          (if (none-left? (first lol))
              empty-list
              (conjoin (count (my-loop first lol)) (average (my-loop next lol)))))
        (average resoltlists))))

;voor port syntax
(define (args->string list-of-strings)
  (define (loop string list)
    (if (none-left? list)
        string
        (loop (string-append (string-append string ".") (car list))(cdr list))))
  (loop (car list-of-strings) (cdr list-of-strings)))

(define (string->args long-string)
  (define stop (string-length long-string))
  (define (loop prev index list)
    (cond ((= index stop) (reverse (cons (substring long-string prev index) list)))
          ((eq? #\. (string-ref long-string index))(loop (+ index 1)
                                                         (+ index 1)
                                                         (cons (substring long-string prev index) list))) ;substring stop op 3de argument (substring "apple" 1 3)-> pp (1ste + 2de , 3de niet)
          (else (loop prev (+ 1 index) list))))
  (loop 0 0 empty-list))