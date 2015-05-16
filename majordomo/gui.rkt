;-----------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------ gui -------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------

#lang racket
(require racket/gui/base)
(require plot)
(require "basic.rkt")
(require "protocol.rkt")
(#%provide make-gui)

;om alle soorten objecten op een lijn te hebben moeten alle objecten dezelfde hoogte hebben, min-height mag nooit overschreden worden
(define temp (new frame% [label "temp"]))
(define spacing (+ 3 (max 
                      (send (new button% [label "NA"][parent temp]) min-height)
                      (send (new message% [label "NA"][parent temp]) min-height)
                      (send (new check-box% [label "NA"][parent temp]) min-height))))

;om te kunnen praten met de majordomo is een link nodig maar als de gui aangemaakt wordt bestaat de majordomo nog niet, deze wordt achteraf toegevoegd
(define (make-gui db log)
  (let ((majordomo '()))
    (define frame (new frame% 
                       [label "energy-monitoring"]
                       ))
    
    ;niet elk device heeft evenveel mogelijkheden toch moet elk onderdeel van een device naast elkaar komen
    ;, voeg een leeg message object toe als een object een bepaalde mogelijkheid mist
    (define (insert-blank-space parent)
      (new message% 
           [parent parent]
           [label " "]
           [min-height spacing]
           ))
    
    (define (clear-area area)
      (send area change-children
            (lambda (children) '())))
    
    ;voor tabs--------------------------------------------------------------------------------------------------------------------------------------------
    ;racket tabs zorgen niet voor het wisselen tussen tabs enkel voor het oproepen van een zekere callback
    ;deze procedures doen dat wel, verder is het ook mogelijk om manueel te switchen tussen tabs en de tabs dan ook werkelijk te laten switchen. 
    (define (tab-callback tab-panel event)
      (send tab-panel change-children
            (lambda (children)
              (list
               (vector-ref tab-vector
                           (send tab-panel
                                 get-selection))))))
    
    (define tab-panel
      (new tab-panel% 
           [parent frame]
           [callback (lambda (tab-panel event)
                       (tab-callback tab-panel event))]
           [choices '()]
           [alignment  '(center top)]
           ))
    
    ;style bepaalt of een area-container vanaf het begin zichtbaar is of niet,
    ;border is zichtbaar en heeft een rand rond zijn data, deleted in onzichtbaar en heeft geen rand
    ;verder wordt ook geen plaats gerezerveerd voor dit gebied. 
    (define steward-tab
      (new vertical-panel% 
           [parent tab-panel]
           [horiz-margin 250]
           ))
    
    (define device-tab
      (new vertical-panel% 
           [parent tab-panel]
           [style '(deleted)]
           [horiz-margin 250]
           )) 
    
    (define overview-tab
      (new vertical-panel% 
           [parent tab-panel]
           [style '(deleted)]
           [horiz-margin 250]
           ))
    
    (define log-tab
      (new vertical-panel% 
           [parent tab-panel]
           [horiz-margin 250]
           [style '(deleted)]
           ))
    
    (define tabs
      (list (cons steward-tab "stewards")
            (cons device-tab "device")
            (cons overview-tab "overview")
            (cons log-tab "log")))
    
    (define tab-names (map cdr tabs))
    (define tab-vector (list->vector (map car tabs)))
    
    (send tab-panel set tab-names)
    
    ;voor error's-------------------------------------------------------------------------------------------------------------
    (define error-dialog (new dialog%
                              [label "error"]
                              [parent frame]
                              ))
    (send error-dialog show #f)
    
    (define error-message
      (new message%
           [label "something went wrong"]
           [parent error-dialog]
           [auto-resize true]))
    
    (define (change-error new-error)
      (send error-message set-label new-error)
      (send error-dialog show #t))
    
    (new button%
         [parent error-dialog]
         [label "ok"]
         (callback (lambda (button event)(send error-dialog show #f)(send frame show #t))))      
    
    
    ;voor plots-------------------------------------------------------------------------------------------------------------
    (plot-x-far-axis? false)  ;teken de boven as niet
    (plot-y-far-axis? false)  ;teken de rechter as niet
    
    (define plot-dialog 0)
    (define (replot list-of-value-lists time-plot)
      (set! plot-dialog
            (parameterize ([plot-width    1000]
                           [plot-height   700]
                           [plot-x-label  "time"]
                           [plot-y-label  "usage"]
                           [plot-x-tick-label-anchor  'top-right]  ;hang labels vast aan hun rechter boven kant
                           [plot-x-tick-label-angle   30]          ;labels vormen een hoek van 30 graden met de x-as
                           )
              (plot-frame (my-loop (lambda (racket-list-of-values)(discrete-histogram (map vector time-plot racket-list-of-values))) list-of-value-lists)
                          #:y-min 0 #:y-max 5)
              ))
      (new button%
           [label "ok"]
           [parent plot-dialog]
           [callback (lambda (button event)(send plot-dialog show false))])
      (send plot-dialog show true))  
    
    ;voor steward-tab----------------------------------------------------------------------------------------------------------
    (define steward-info-panel (new horizontal-panel% ;kinderen staan naast elkaar niet onder elkaar
                                    [parent steward-tab]
                                    [style (list 'border)]
                                    [min-width 800]
                                    [min-height 600]
                                    ))
    
    (define steward-names-panel (new vertical-panel% 
                                     [parent steward-info-panel]
                                     ))
    
    (define steward-addresses-panel (new vertical-panel% 
                                         [parent steward-info-panel]
                                         ))
    
    (define steward-plot-panel (new vertical-panel%
                                    [parent steward-info-panel]
                                    )) 
    
    (define steward-delete-panel (new vertical-panel%
                                      [parent steward-info-panel]
                                      ))  
    ;wordt na elke delete opgeroepen
    (define (make-steward-panel)
      (send steward-choice append "           ")
      (let ((stewards (send-message db 'get-all-stewards)))
        (my-loop (lambda (steward)(add-steward steward)) stewards)))
    
    (define (add-steward steward)
      (new message% 
           [parent steward-names-panel]
           [label steward]
           [min-height spacing]
           )
      
      (new message% 
           [parent steward-addresses-panel]
           [label (send-message db 'get-steward-address steward)]
           [min-height spacing]
           )
      
      (new button%
           [parent steward-plot-panel]
           [label "plot"]
           [callback (lambda (b e)
                       (let 
                           ((devices-data (send-message db 'get-steward-data steward))
                            (time (send-message db 'get-steward-time steward)))
                         (if time
                             (replot (list devices-data)  time)
                             (send error-dialog show true))))]
           [min-height spacing])
      
      (new button% 
           [parent steward-delete-panel]
           [label "delete"]
           [callback (lambda (button event)
                       (clear-area steward-names-panel)
                       (clear-area steward-addresses-panel)
                       (clear-area steward-delete-panel)
                       (clear-area steward-plot-panel)
                       (send steward-choice clear)
                       (clear-area choicepanel)
                       (send-message majordomo 'erase-steward steward)
                       (send-message log 'add-log-value (string-append "user deleted steward: " steward))
                       )]
           [min-height spacing]
           )
      
      (send steward-choice append steward)
      (make-steward-box steward)
      (add-devices-to-steward-box steward (send-message db 'get-specific-devices steward))
      )    
    
    (define (add-steward-button)
      (define steward-add-dialog (new dialog%
                                      [label "add-steward"]
                                      [parent frame]
                                      ))    
      (send steward-add-dialog show #f)
      
      (define steward-name-field (new text-field% [parent steward-add-dialog] [label "steward name:"]
                                      (callback (lambda (e b) (send steward-name-field get-value)))))
      
      (define steward-address-field (new text-field% [parent steward-add-dialog] [label "steward address:"]
                                         (callback (lambda (e b) (send steward-name-field get-value)))))
      
      (define steward-dialog-panel (new horizontal-panel%
                                        [parent steward-add-dialog]
                                        [style '(deleted)]))
      
      (new button%
           [parent steward-add-dialog]
           [label "cancel"]
           (callback (lambda (button event)
                       (send steward-name-field set-value "")
                       (send steward-address-field set-value "")
                       (send steward-add-dialog show #f)
                       (send frame show #t))))
      
      (new button%
           [parent steward-add-dialog]
           [label "ok"]
           (callback (lambda (button event)
                       (let ((steward-name (send steward-name-field get-value))
                             (steward-address (send steward-address-field get-value)))
                         (unless (steward-failed? steward-name steward-address)
                           (send-message db 'put-steward steward-name steward-address)
                           (send-message majordomo 'add-steward steward-name)
                           (send steward-add-dialog show #f)
                           (add-steward steward-name)
                           (send steward-name-field set-value "")
                           (send steward-address-field set-value "")
                           (send-message log 'add-log-value (string-append "user added steward: " steward-name ", with address: " steward-address))                                  
                           )))))
      
      (define (steward-failed? steward-name steward-address)
        (cond ((null? (string-length steward-name))
               (change-error "name has to be at least one character long"))
              ((null? (string-length steward-address))
               (change-error "address has to be at least one character long"))
              ((send-message db 'steward-name-used? steward-name)
               (change-error "name has to be unique"))
              ((send-message db 'steward-address-used? steward-address)
               (change-error "address has to be unique"))
              (else #f)
              ))
      
      (new button%
           [parent steward-tab]
           [label "add-steward"]
           (callback (lambda (button event)(send steward-add-dialog show #t)))         
           )
      )
    
    ;voor device-tab----------------------------------------------------------------------------------------------------
    (define steward-choice (new choice%
                                [label "choice steward"]
                                [parent device-tab]
                                [choices '()]
                                [callback (lambda (button event)
                                            (clear-devices)
                                            (add-devices))]
                                ))
    
    (define (get-choice)
      (send steward-choice get-string (send steward-choice get-selection)))
    
    (define device-panel (new horizontal-panel% 
                              [parent device-tab]
                              [style (list 'border)]
                              ))
    
    (define device-name-panel (new vertical-panel% 
                                   [parent device-panel]
                                   ))
    
    (define device-product-id-panel (new vertical-panel% 
                                         [parent device-panel]
                                         ))
    
    (define device-switch-panel (new vertical-panel% 
                                     [parent device-panel]
                                     ))
    (define device-plot-panel (new vertical-panel% 
                                   [parent device-panel]
                                   ))
    (define device-delete-panel (new vertical-panel% 
                                     [parent device-panel]
                                     ))    
    
    (define (add-devices)
      (let ((steward (get-choice)))
        (my-loop (lambda (device) (add-device device)) (send-message db 'get-specific-devices steward))))
    
    (define (clear-devices)
      (clear-area device-name-panel)
      (clear-area device-product-id-panel)
      (clear-area device-switch-panel)
      (clear-area device-delete-panel)
      (clear-area device-plot-panel)
      )  
    
    (define (add-device device-name)
      (let* ((steward (get-choice))
             (protocol (send-message db 'get-protocol steward device-name)))
        ;device naam
        (new message% 
             [label device-name]
             [parent device-name-panel]
             [min-height spacing]
             )
        
        ;device product-id
        (new message%
             [label (send-message db 'get-device-product-id steward device-name)]
             [parent device-product-id-panel]
             [min-height spacing]
             )
        
        ;device aan/uit
        (if (switch? protocol)
            (new check-box%	 
                 [label "turned-on"]	 
                 [parent device-switch-panel]	 
                 [callback (lambda (button event)
                             (if (send-message majordomo 'switch-device steward device-name (send button get-value))
                                 (send-message log 'add-log-value (string-append "user switched device: " device-name ", from steward: " steward))                                 
                                 (change-error "device-unreachable"))
                             )]
                 [min-height spacing]
                 [value (if (send-message majordomo 'device-turned-on? steward device-name)
                            true
                            false)]
                 )
            (insert-blank-space device-switch-panel))
        
        ;device plot
        (if (usage? protocol)
            (new button%
                 [parent device-plot-panel]
                 [label "plot"]
                 [callback (lambda (b e)
                             (replot (list (send-message db 'get-data steward device-name))
                                     (send-message db 'get-time steward device-name)))]
                 [min-height spacing])
            (insert-blank-space device-plot-panel))
        
        ;device delete
        (new button% 
             [parent device-delete-panel]
             [label "delete"]
             [callback (lambda (button event)
                         (clear-devices)                        
                         (send-message majordomo 'erase-device steward device-name)
                         (clear-steward-box steward)
                         (add-devices)
                         (send-message log 'add-log-value (string-append "user deleted device: " device-name ", from steward: " steward))
                         )]
             [min-height spacing]
             )
        ))
    
    (define (add-device-button)
      (define device-add-dialog (new dialog%
                                     [label "add-device"]
                                     [parent frame]
                                     ))    
      (send device-add-dialog show #f)
      
      (define device-name-field (new text-field% [parent device-add-dialog] [label "device name:"]
                                     ))
      
      (define product-id-field (new text-field% [parent device-add-dialog] [label "product-id:"]
                                    ))            
      
      (define device-protocol-boxes (make-protocol-boxes device-add-dialog))
      
      (define device-dialog-panel (new horizontal-panel%
                                       [parent device-add-dialog]
                                       [style '(deleted)]))
      
      (new button%
           [parent device-add-dialog]
           [label "cancel"]
           (callback (lambda (button event)
                       (send device-name-field set-value "")
                       (send product-id-field set-value "")
                       (empty-protocol-boxes device-protocol-boxes)        
                       (send device-add-dialog show #f)
                       (send frame show #t))))
      
      (new button%
           [parent device-add-dialog]
           [label "ok"]
           (callback (lambda (button event)
                       (let ((device-name (send device-name-field get-value))
                             (product-id (send product-id-field get-value))
                             (device-protocol (calc-protocol device-protocol-boxes))
                             (steward-name (get-choice)))                         
                         (if (and (device-failed? device-name product-id device-protocol steward-name)
                                  (send-message majordomo 'add-device steward-name device-name device-protocol))                            
                             (begin (send-message db 'put-device steward-name device-name product-id device-protocol)           
                                    (send device-add-dialog show #f) 
                                    (add-device device-name)
                                    (send device-name-field set-value "")
                                    (send product-id-field set-value "")
                                    (empty-protocol-boxes device-protocol-boxes)
                                    (send-message log 'add-log-value (string-append "user added device: " device-name
                                                                                    " with product-id " product-id ", to steward: " steward-name))
                                    (add-devices-to-steward-box steward-name (list device-name)))
                             (change-error "device unreachable"))))))
      
      (define (device-failed? device-name product-id device-protocol steward-name)
        (cond ((null? (string-length device-name))
               (change-error "name has to be at least one character long"))              
              ((equal? 14 (string-length product-id))
               (change-error "product-id has to be exaclty 14 character long"))
              ((equal? device-protocol 1)
               (change-error "at least one option has to be selected"))
              ((send-message db 'device-name-used? device-name steward-name)
               (change-error "name has to be unique"))
              ((send-message db 'product-id-used? product-id steward-name)
               (change-error "product-id has to be unique"))
              (else #f)
              ))
      
      (new button%
           [parent device-tab]
           [label "add-device"]
           (callback (lambda (button event)(send device-add-dialog show #t)))         
           )
      )
    
    
    ;voor overview-tab------------------------------------------------------------------------------------------------------------------------------ 
    (define (first-steward-name x) (caar x))
    (define (first-steward-box x) (cdar x))    
    
    (define choicepanel (new horizontal-panel%
                             [parent overview-tab]
                             [alignment (list 'center 'top)]
                             [min-width 800]
                             [min-height 200]   
                             ))
    
    (define steward-box-list '())
    
    (define (make-steward-box steward-name)
      (let ((box
             (new list-box% [parent choicepanel]
                  [label steward-name]
                  [choices null]
                  [style (list 'extended  'vertical-label)]
                  )))
        (set! steward-box-list (conjoin (conjoin steward-name box) steward-box-list))))
    
    (define (find-steward-box steward)
      (define (loop lst)
        (cond 
          ((null? lst) #f)
          ((equal? (first-steward-name lst) steward) (first-steward-box lst))
          (else (loop (next lst)))))
      (loop steward-box-list))
    
    (define (add-devices-to-steward-box steward devices-list)
      (let ((steward-box (find-steward-box steward)))
        (my-loop (lambda (x)(when (usage? (send-message db 'get-protocol steward x))(send steward-box append x))) devices-list)))
    
    (define (clear-steward-box steward)
      (add-devices-to-steward-box steward (send-message db 'get-specific-devices steward))
      )
    
    (define (get-selection)
      (my-flatten (my-loop (lambda (x)
                             ;maak per device een tupple steward device
                             (my-loop (lambda (y)
                                        (conjoin (first x) y))
                                      ;neem per device de naam van het device
                                      (my-loop (lambda (z)
                                                 (send (next x) get-string z))
                                               ;neem per steward de geselecteerde devices
                                               (send (next x) get-selections)))) steward-box-list)))
    
    (new button%
         [parent overview-tab]
         [label "plot"]
         [callback (lambda (b e)
                     (if (none-left? (get-selection))
                         (send error-dialog show #t)
                         (let* ((selection (get-selection))
                                (steward (first (first selection)))
                                (first-device (next (first selection)))
                                (devices-data (average-lists (my-loop (lambda (x) (send-message db 'get-data (first x)(next x))) selection)))
                                (time-list (first (shrink (list devices-data (send-message db 'get-time steward first-device))))))
                           (replot (list devices-data) time-list)
                           ))
                     )]
         [min-height spacing])
    
    (new button%
         [parent overview-tab]
         [label "delete data"]
         [callback (lambda (b e)
                     (let* ((selection (get-selection))
                            (tables (my-loop (lambda (x)(string-append (first x)(next x))) selection)))                          
                       (my-loop (lambda (x)(send-message db 'erase-table-data x)) tables)))]
         [min-height spacing])
    
    
    ;voor log tab--------------------------------------------------------------------------------------------------------------------------------
    (new button%
         [label "refresh"]
         [parent log-tab]
         [callback (lambda (button event)(send log-list set  (send-message log 'return-log)))]
         )
    
    (define log-list (new list-box%
                          [parent log-tab]
                          [label #f]
                          [choices null]
                          [min-width 800]
                          [min-height 600]
                          ))
    
    
    ;voor gui functionaliteit --------------------------------------------------------------------------------------------------------------------------------
    
    (define (initiate majord logvalues)
      (set! majordomo majord)
      (add-steward-button)
      (add-device-button)
      (send log-list set logvalues)
      )
    
    (define (dispatch message)
      (case message
        ((make-steward-panel) make-steward-panel)
        ((initiate) initiate)
        (else (error "gui unknow message ~a" message))
        ))
    
    (send frame maximize #t)
    (send frame show #t)
    dispatch
    )
  )