;-----------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------ database -------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------

#lang racket
(require (for-syntax racket/match/parse))
(require db)
(require "basic.rkt")
(require "protocol.rkt")
(#%provide make-db-connection)


(define (make-db-connection)
  (let (
        (sql (sqlite3-connect #:database "project2.db"))
        )
    
    ;probeert een device toe te voegen als sql een error geeft word er #f terug gegeven anders #t
    (define (put-device steward device product-id protocol) 
      (if (element-of? steward (query-list sql "SELECT steward FROM stewards"))
          (void? (with-handlers ([exn:fail:sql? exn:fail:sql-info]) 
                   (query-exec sql 
                               "INSERT INTO devices VALUES ($1, $2, $3, $4)"
                               steward
                               device
                               product-id
                               protocol
                               )
                   ;als het device een verbruik meter heeft voeg dan ook een tabel toe om deze data in op te slaan
                   (when (usage? protocol)
                   (create-data-table steward device))))
          #f))
    
    (define (device-name-used? name steward)
      (element-of? name (query-list sql "SELECT device FROM devices WHERE steward=$1"
                                    steward)))
    
    (define (product-id-used? id steward)
      (element-of? id (query-list sql "SELECT productid FROM devices")))    
    
     ;probeert een steward toe te voegen als sql een error geeft word er #f terug gegeven anders #t
    (define (put-steward steward address)
      (void? (with-handlers ([exn:fail:sql? exn:fail:sql-info])
               (query-exec sql "INSERT INTO stewards VALUES ($1, $2)"
                           steward
                           address))))
    
    (define (steward-name-used? name)
      (element-of? name (query-list sql "SELECT steward FROM stewards")))
    
    (define (steward-address-used? address)
      (element-of? address (query-list sql "SELECT physical_address FROM stewards")))
    
    (define (get-all-stewards)
      (query-list sql "SELECT steward FROM stewards ORDER BY steward ASC"))
    
    ;map omdat het gaat over het racket object lijst
    ;query-rows geeft een lijst van vectoren terug
    (define (get-steward-device-list)
      (map vector->mylist (query-rows sql "SELECT steward, device FROM devices")))
    
    (define (get-all-devices-with-steward steward)
      (query-list sql "SELECT device FROM devices WHERE steward=$1"
                  steward))
    
    (define (erase-device-db steward device)
      ;de tabel enkel verwijderen als de tabel is aangemaakt
      (when (usage? (get-protocol steward device)) (query-exec sql (string-append "DROP TABLE " (data-table-name steward device))))
      (query-exec sql "DELETE FROM devices WHERE steward=$1 AND device=$2" 
                  steward
                  device)
      )
    
    ;verwijder een steward en al zijn devices
    (define (erase-steward-db steward)
      (let ((devices (get-all-devices-with-steward steward)))             
      (query-exec sql "delete FROM stewards WHERE steward=$1" 
                  steward)
      (my-loop (lambda(x)(erase-device-db steward x)) devices)))
    
    (define (get-steward-address steward)
      (car (query-list sql "SELECT physical_address FROM stewards WHERE steward=$1"
                       steward)))
    
    (define (get-device-product-id steward device)
      (car (query-list sql "SELECT productid FROM devices WHERE steward=$1 AND device=$2"
                       steward
                       device)))
    
    (define (get-protocol steward device)
      (car (query-list sql "SELECT protocol FROM devices WHERE steward=$1 AND device=$2"
                       steward
                       device)))
    
    (define (add-data steward time data-package)
      (let ((device (get-device data-package))
            (data (get-data data-package)))
      (query-exec sql (string-append (string-append "INSERT INTO "(data-table-name steward device)) " VALUES ($1, $2)")
                  time
                  data)))
    
    (define (create-data-table steward device)
      (query-exec sql 
                  (string-append (string-append "CREATE TABLE " (data-table-name steward device))
                                 " (time STRING NOT NULL PRIMARY KEY, value INTEGER NOT NULL)")
                  ))
    
    (define (select-data steward device)
      (query-list sql (string-append "SELECT value FROM " (data-table-name steward device)))                   
      )
    
    (define (select-time steward device)
      (query-list sql (string-append "SELECT time FROM " (data-table-name steward device)))                   
      )
    
    (define (select-steward-data steward)
      (let ((devices (get-all-devices-with-steward steward))
            (resolt-list empty-list))
        (my-loop (lambda (x) (when (usage? (get-protocol steward x))
                                    (set! resolt-list (conjoin (select-data steward x) resolt-list)))) devices)
        (average-lists resolt-list)
        ))
    
    ;om er zekere van te zijn dat ik de tijd op vraag van een bestaande tabel
    (define (select-steward-time steward)
     (define (loop lst)
          (cond ((none-left? lst) #f)
                ((usage? (get-protocol steward (first lst))) (select-time steward (first lst)))
                (else (loop (next lst)))))
        (loop (get-all-devices-with-steward steward)))
    
    (define (erase-table-data table)
      (query-exec sql (string-append "DELETE FROM " table)))
    
    (define (dispatch message)
      (case message
        ((put-device) put-device)
        ((device-name-used?) device-name-used?)
        ((product-id-used?) product-id-used?)
        ((put-steward) put-steward)
        ((steward-name-used?) steward-name-used?)
        ((steward-address-used?) steward-address-used?)
        ((get-all-stewards) get-all-stewards)
        ((get-specific-devices) get-all-devices-with-steward)
        ((get-steward-device-list) get-steward-device-list)
        ((get-steward-address) get-steward-address)
        ((get-device-product-id) get-device-product-id)
        ((get-protocol) get-protocol)
        ((erase-device) erase-device-db)
        ((erase-steward) erase-steward-db)
        ((add-data) add-data)
        ((get-data) select-data)
        ((get-time) select-time)
        ((get-steward-data) select-steward-data)
        ((get-steward-time) select-steward-time)
        ((erase-table-data) erase-table-data)
        (else (error 'database "unknown message ~a" message))
        ))
    dispatch        
    ))

;zet een paar standaard waarden om makkelijk te kunnen testen
(define (reflock)
  (define db (make-db-connection))
  (send-message db 'put-steward "steward1" "slaapkamer")
  (send-message db 'put-steward "steward2" "badkamer")
  (send-message db 'put-steward "steward3" "keuken")
  (send-message db 'put-device "steward1" "device1" "ZBS110V2120901" 2)
  (send-message db 'put-device "steward1" "device2" "ZBS110V2120902" 6)
  (send-message db 'put-device "steward1" "device3" "ZBS110V2120903" 6)
  (send-message db 'put-device "steward2" "device4" "ZBS110V2120904" 3)
  (send-message db 'put-device "steward2" "device5" "ZBS110V2120905" 3)
  (send-message db 'put-device "steward2" "device6" "ZBS110V2120906" 6)
  (send-message db 'put-device "steward3" "device7" "ZBS110V2120907" 6)
  (send-message db 'put-device "steward3" "device8" "ZBS110V2120908" 2)
  (send-message db 'put-device "steward3" "device9" "ZBS110V2120909" 6) 
  )