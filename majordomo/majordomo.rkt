;-----------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------ majordomo -------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------

;Deze file is de echte majordomo en wordt enkel gebruikt in de eigenlijke applicatie, niet in de simulatie

#lang racket
(require "basic.rkt")
(require "database.rkt")
(require "gui.rkt")
(require "logfile.rkt")

(define (make-majordomo simulation)  
  ;abstracties
  (define (majordomo-node name in-port out-port)(vector name (make-port-pair in-port out-port)))
  (define (get-name x)(vector-ref x 0))
  (define (get-port-pair x)(vector-ref x 1))
  (define (first-steward-inpt x)(car (car x))) ;eerste steward bij instantieeren van majordomo
  (define (first-devices-inpt x) (car (cdr (car x)))) ;eerste device bij instantieeren van majordomo
  
  (let* (
         (stewards empty-list); in stewards zit een lijst van stewards-ids gekoppelt aan hun tcp-poorten
         (db (make-db-connection))
         (log (make-log-file))
         (gui (make-gui db log))
         (port-bussy #f);houdt bij of de de askerthread op dit moment bezig is data op te vragen
         (listener (tcp-listen 6666))
         )
    
    ;setters-------------------
    (define (add-steward steward-name)
      (let-values (((in out) (tcp-accept listener)))
        (set! stewards (conjoin (majordomo-node steward-name in out) stewards))))
    
    (define (erase-steward-majordomo steward)
      (send-message db 'erase-steward steward)
      (set! stewards (erase-by-copy stewards steward (lambda (stew-list stew-name) (equal? (get-name (first stew-list)) stew-name))))
      (start-up-gui))
    
    (define (add-device steward-name device protocol product-id)
      (let ((steward (find-steward steward-name)))
        (my-write (args->string (list "add-device" device (number->string protocol) product-id)) (get-port-pair steward))
        (equal? (my-read (get-port-pair steward)) "device added"))
      )
    
    (define (erase-device-majordomo steward-name device)
      (let ((steward (find-steward steward-name)))
        (send-message db 'erase-device steward-name device)
        (my-write (args->string (list "erase-device" device)) (get-port-pair steward))))
    
    ;eigenlijke functies van majordomo----------------------------------------------------------
    ;vind de majordomo-node van de steward
    (define (find-steward steward-name)
      (define (loop lst)
        (cond ((none-left? lst)  (error "non-existing steward"))
              ((equal? (get-name (first lst)) steward-name) (first lst))
              (else (loop (next lst)))))
      (loop stewards))
    
    ;wacht met de uitvoering van een procedure tot de poort vrij is
    (define (wait-for-free lambda-function)
      (if port-bussy
          (wait-for-free lambda-function)
          (begin 
            (thread-suspend asker-thread)
            (let ((return-value (lambda-function)))
              (thread-resume asker-thread)
              return-value)
            )))
    
    (define (switch-device steward-name device on)
      (let ((steward (find-steward steward-name)))
        (wait-for-free (lambda ()
                         (if on 
                             (begin (my-write (args->string (list "turn-on" device))(get-port-pair steward))
                                    (equal? (my-read (get-port-pair steward)) "turned-on"))  
                             (begin (my-write (args->string (list "turn-off" device))(get-port-pair steward))
                                    (equal? (my-read (get-port-pair steward)) "turned-off")))))))
    
    (define (device-turned-on? steward-name device)
      (let ((steward (find-steward steward-name)))
        (wait-for-free (lambda ()
                         (my-write (args->string (list "turned-on?" device))(get-port-pair steward))
                         (equal? (my-read (get-port-pair steward)) "ON")))))
    
    (define (start-up-majordomo) ;haal alle data uit de database en instatier deze
      (let (
            (stewards (send-message db 'get-all-stewards))
            (stewards-devices (send-message db 'get-steward-device-list)) ;lijst steward-device, steward-device, ...
            )
        
        (define (add-all-devices lst)
          (unless (none-left? lst)
            (let ((steward-name (first-steward-inpt lst))
                  (device (first-devices-inpt lst)))
              ;op dit moment bestaan de stewards al
              (add-device steward-name device (send-message db 'get-protocol steward-name device)(send-message db 'get-device-product-id steward-name device))
              (add-all-devices (next lst)))))
        
        (define (add-all-stewards lst)
          (if (none-left? lst)
              (add-all-devices stewards-devices)
              (begin 
                (add-steward (first lst))
                (add-all-stewards (next lst)))))
        (add-all-stewards stewards)
        ))
    
    ;de gui vraagt aan de database om alle stewards op te halen
    (define (start-up-gui)
      (send-message gui 'make-steward-panel))
    
    ;wordt één maal uitgevoerd bij het opstarten van de code
    (define (initiate)
      (start-up-majordomo)
      (start-up-gui)
      (send-message gui 'initiate dispatch (send-message log 'return-log)))
    
    
    (define (get-values stewards)
      (unless (none-left? stewards)
        (my-write "get-usage-all" (get-port-pair (first stewards)))
        (my-loop ;r5rs cons is de mutable cons uit racket
         (lambda (x)(send-message db 'add-data (get-name (first stewards))(send-message log 'get-time) x))
         (my-read (get-port-pair (first stewards))))
        (get-values (next stewards))))
    
    ;vraagt elke 60 seconden aan alle steward om alle data op te vragen 
    (define (get-all-values)
      (set! port-bussy #f)
      (sleep 60)
      (set! port-bussy #t)
      (get-values stewards)      
      (get-all-values))
    
    (define asker-thread (thread get-all-values))    
    
    (define (dispatch message)
      (case message
        ((add-steward) add-steward)
        ((add-device) add-device)
        ((erase-steward) erase-steward-majordomo)
        ((erase-device) erase-device-majordomo)
        ((switch-device) switch-device)
        ((device-turned-on?) device-turned-on?)
        ((initiate) initiate)  
        (else (error 'majordomo "unknown message ~a" message))))
    dispatch))


(define majordomo (make-majordomo #t))
(send-message majordomo 'initiate)
