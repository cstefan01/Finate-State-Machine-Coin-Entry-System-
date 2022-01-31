
#lang racket
(require 2htdp/image)


#|
  ======================================== 
  |             GROUP PROJECT            |
  ========================================
  |               Students:              |
  |--------------------------------------|                                      |
  |           CLAUDIU-IONUT STEFAN       |
  |         JUSTIN WELLS-NECZYPORCZUK    |
  |              SAMUEL SAMIA            |
  |             ZAHIDUL ISLAM            |
  ========================================
  | Create a program that will stimulate |
  |       the behaviour of an FSM.       |
  ========================================
|#



;; The entire FSM model represented as a list of lists containing the (1-state, event, 2-state(next-state) )
;==============================================================
(define state-Table
  ; (state event) -> state
  '(
    (("0.00p" "5p") "0.05p")
    (("0.00p" "10p") "0.10p") 
    (("0.00p" "20p") "0.20p")
    (("0.00p" "50p") "0.50p")
    (("0.00p" "100p") "£1.00")

    (("0.05p" "5p") "0.10p")
    (("0.10p" "10p") "0.20p")
    (("0.10p" "20p") "0.30p")
    (("0.10p" "50p") "0.60p")
    
    (("0.20p" "10p") "0.30p")
    (("0.20p" "20p") "0.40p")
    (("0.20p" "50p") "0.70p")
    
    (("0.30p" "10p") "0.40p")
    (("0.30p" "20p") "0.50p")
    (("0.30p" "50p") "0.80p")
    
    (("0.40p" "10p") "0.50p")
    (("0.40p" "20p") "0.60p")
    (("0.40p" "50p") "0.90p")
    
    (("0.50p" "10p") "0.60p")
    (("0.50p" "20p") "0.70p")
    (("0.50p" "50p") "£1.00")
    
    (("0.60p" "10p") "0.70p")
    (("0.60p" "20p") "0.80p")
    
    (("0.70p" "10p") "0.80p")
    (("0.70p" "20p") "0.90p")
    
    (("0.80p" "10p") "0.90p")
    (("0.80p" "20p") "£1.00")
    
    (("0.90p" "10p") "£1.00")
    
    ))
;==============================================================

             

(define next-state? (λ (init-state event stateTable)
                      (cond
                         ((null? stateTable) #f )
                         ((equal? (caar stateTable) (list init-state event)) #t)
                         (else
                           (next-state? init-state event (cdr stateTable))))))

                         
(define next-state (λ (init-state event stateTable)
                      (cond
                         ((null? stateTable) #f )
                         ((equal? (caar stateTable) (list init-state event)) (car (reverse (car stateTable)))) 
                         (else
                           (next-state init-state event (cdr stateTable))))))                     
                        

; run-sequence: initial-state, sequence of events, stateTable -> end-up state
;==============================================================
(define run-sequence (λ (init-state event-seq stateTable)
                       (cond
                         ((null? event-seq) #f)
                         (else 
                          (let ([current-state  (next-state init-state (car event-seq) stateTable)])
                            (println current-state)
                            (sleep 1.3)
                            (run-sequence current-state (cdr event-seq) stateTable)
                            )))))
                        
                                                  
; ____________________                         
;|SOB: 7,8,101,102,143|
;==============================================================                               
                         
                  
(define show-sequence (λ (init-state event-seq stateTable)
                       (cond
                         ((null? event-seq) #f)
                         (else 
                          (let ([current-state  (next-state? init-state (car event-seq) stateTable)])
                            (println (circle 30 "outline" "black"))
                            (println (text current-state 25 "red"))
                            (display "\n")
                            (println (text "searching next state.." 25 "black"))
                            (sleep 2)
                            (display "\n")
                            (show-sequence current-state (cdr event-seq) stateTable)
                            )))))


                           

                   
                  
                  
                                               
                               
;(run-sequence "0.00p" '("10p" "10p") state-Table)

;(run-sequence (show-sequence "0.00p" '("10p" "10p") state-Table) '("10p" "10p") state-Table)                           
                         
                        
                         
                         

                           
                           


                          
                          
                          
                         
                        
                        
                         
                         
                      
                       




                       
                       
                     
                         
                        

