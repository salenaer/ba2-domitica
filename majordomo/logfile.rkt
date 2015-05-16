;-----------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------ logfile -------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------


#lang racket
(require racket/date)
(require "basic.rkt")
(#%provide make-log-file)
(define (make-log-file)
  
  (let (
        ( kFilePath "logfile.txt")
        
        ;vector van lengte 302
        ;op adress 301 staat waar we in de log file gaan schrijven
        ;op adress 300 staat waar de laatste zinvolle data is geschreven
        ;de hoogste plek waar we naar willen schrijven is 299 (0 -> 299 = 300 plaatsen)
        (limit 300) 
        (current 0)
        (last 0)
        )
    
    (define (write-log)
      (let ((file (open-output-file kFilePath #:exists 'replace)))
        (vector-set! logvalues 300 current)
        (vector-set! logvalues 301 last)
        (write logvalues file)
        (close-output-port file))
      )
    
    (define (read-log)
      (let* ((file (open-input-file kFilePath))
             (data (read file)))
        (close-input-port file)
        (set! current (vector-ref data 300))
        (set! last (vector-ref data 301))
        data))
    
    (define logvalues (read-log))
    
    (define (add-log-value value)
      (let ((timed-value (string-append (string-append (insert-current-time) " ") value)))
        (vector-set! logvalues current timed-value)
        (unless (= last 299)
            (set! last (+ last 1)))
        (set! current (modulo (+ current 1) limit))
        timed-value)
      (write-log))
    
    ;geeft de lop op chronologische volgorde terug van nieuwste naar oudste
    (define (return-log)
      (reverse (append (vector->list (vector-copy logvalues current last)) 
              (vector->list (vector-take logvalues current)))))
    
    
    ;geeft de tijd terug als string
    (define (insert-current-time)
      (let* (
             (time (current-date))
             (minute (number->string (date-minute time)))
             (hour (number->string (date-hour time)))
             (day (number->string (date-day time)))
             (month (number->string (date-month time)))
             (year (number->string (date-year time))))
        (string-append hour ":" minute " " day "/" month "/" year)))
    
   (define (dispatch message)
      (case message
        ((add-log-value) add-log-value)
        ((return-log) return-log)
        ((get-time) insert-current-time)
        (else (error 'log-file "unknown message ~a" message))))
    dispatch))
