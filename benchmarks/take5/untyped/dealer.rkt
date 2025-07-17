#lang racket/base

;; the dealer and supervisor of the deck

(provide
 ;; [Listof Player] -> Dealer
 ;; create a dealer object that connects the players with the deck
 ;; and places the player's chosen cards
 create-dealer)

(require
  racket/list
  require-typed-check
  racket/class)
(require "card.rkt")
(require (only-in "basics.rkt"
  FACE
  FIVE
  STACKS
  SIXTYSIX
  HAND
  MIN-BULL
  MAX-BULL
  configuration
))
(require (only-in "card-pool.rkt"
  create-card-pool
))
(require (only-in "deck.rkt"
  create-deck
))
(require (only-in "player.rkt"
  player%
))
;; ---------------------------------------------------------------------------------------------------

;; TODO should not need to supply this
(define (default-order loc)
  (sort loc > #:key card-face))

(define (default-faces)
  MIN-BULL)

;; Note to self: the types for the below descriptions are used out of scope for now
;; in a file-module they come back into scope 

(define (create-dealer players)
  (new dealer% [players players]))

(define dealer%
  (class object%
    (init-field
     players)

    (super-new)

    (field
     [internal%
      (class player%
        (init-field player)
        (super-new [n (send player name)] [order default-order])
        (field [my-bulls 0])
        (define/public (bulls) ;; gets the bull score
          (get-field my-bulls this))
        (define/public (add-score n) ;; mutates the bull score
          (set-field! my-bulls this (+ n (get-field my-bulls this)))))]
     [internals (for/list
                          ([p (in-list (get-field players this))])
                  (new internal% [player p]))])

    ;; ---------------------------------------------------------------------------------------------
    ;; running a game 

    (define/public (play-game (shuffle values) (faces default-faces))
      (define n (length (get-field internals this)))
      (when (> (+ (* n HAND) STACKS) FACE)
        (error 'play-game "cannot play with ~a players; more cards needed" n))

      (let play-game  ([i  1])
        (play-round shuffle faces) ;; calling play round
        (if (any-player-done?)
            (present-results i)
            (play-game (+ i 1))))) 

    (define/public (present-results i)
      (define sorted
        (sort
         (get-field internals this) < #:key (lambda (i ) (send i bulls))))
      `((after-round ,i)
        ,(for/list
            ([p  (in-list sorted)])
           `(,(send p name) ,(send p bulls)))))

    (define/public (any-player-done?)
      (for/or
              ((p  (in-list (get-field internals this))))
        (> (send p bulls) SIXTYSIX)))

    (define/public (play-round shuffle faces)
      (define card-pool (create-card-pool shuffle faces));; call create-card-pool :: card-pool.rkt
      (define deck (create-deck card-pool)) ;; create deck :: in deck.rkt
      (deal-cards card-pool);; call deal-cards
      (for ((p HAND))
        (play-turn deck)))

    (define/private (deal-cards card-pool)
      (for ((p  (in-list (get-field internals this)))) ;; p is a person class
        (send p start-round (send card-pool draw-hand)))) ;; call draw-hand is card-pool.rkt, I believe this orders the cards for each player



    
    (define/private (play-turn deck)
      (define played-cards
        (for/list
                  ((p  (in-list (get-field internals this))))
          (list p (send p start-turn deck))))
      ;(printf "played_cards: ~a\n" played-cards)
      (define sorted-played-cards
        (sort played-cards < #:key (lambda (x) (card-face (second x)))))
      (place-cards deck sorted-played-cards))

    (define/private (place-cards deck sorted-player-cards)
      (for ((p+c  (in-list sorted-player-cards)))
        (define player (first p+c))
        (define card (second p+c))
        (cond
          [(send deck larger-than-some-top-of-stacks? card)
           (define closest-fit-stack (send deck fit card))
           (cond
             [(< (length closest-fit-stack) FIVE)
              (send deck push card)]
             [(= (length closest-fit-stack) FIVE)
              (define bulls (send deck replace closest-fit-stack card))
              (send player add-score bulls)])]
          [else ;; the tops of all stacks have larger face values than card
           (define chosen-stack (send player choose deck))
           (define bulls (send deck replace chosen-stack card))
           (send player add-score bulls)])))

))





;; testing suite


(module+ test
  (require typed/rackunit)
  

  ;; Intergration Test: Card-pool, Stack, & Bulls
  ;; -- Check that the card-pool is created correctly, but also does not contain repeats, in other words, that the cards dealt are not repeated within a round, but within a game is acceptable
  ;; -- Checks that the stack has operations applied under correct circumstances, i.e, replace is called after push has been called 4 times
  ;; -- Checks that the bulls number added is correct, it is updated for the player correctly

  (define (create-player-loc i (order default-order))
  (new player% [n i] [order order]))

  (define players (build-list 6 create-player-loc))

  (define dealer1 (create-dealer players))


  (define shuffles values)
  (define faces default-faces)

  (define card-pool (create-card-pool shuffle faces))
  (define deck (create-deck card-pool))
  ;(printf "card-pool: ~a\n" card-pool)
  ;(printf "deck: ~a\n" deck)


  (define init-shuffle (get-field shuffle card-pool))
  (define init-RB (get-field random-bulls card-pool))
  
  (define my-cards
      (init-shuffle (build-list FACE (lambda (i) (card (+ i 1) (init-RB))))))
  ;(printf "my_cards: ~a\n" my-cards)

  ;; -----------------------------------------------------------------------------------------------
  (define hand-list '())
  ;; initialize 
  (define (deal-cards-loc card-pool)
      (for ((p  (in-list (get-field internals dealer1)))) ;; p is a person class
        (define pl-hand (send card-pool draw-hand))
        ;; as the hands are distrubed, test that there are not repeated cards
        (check-equal? (uniqueness pl-hand) #t)
        (set! hand-list (cons pl-hand hand-list))
        (send p start-round pl-hand))) 
  ;; collect all initial cards, make sure they are all unique through recursion
  (define (uniqueness cards)
    (cond
      [(equal? cards '()) #t]
      [(not (equal? (member (first cards) (rest cards)) #f)) #f]
      [else (uniqueness (rest cards))])

    )
  
  (define unique-test (uniqueness my-cards))

  (check-equal? unique-test #t)
  (deal-cards-loc card-pool)
  
  (define curr-internals (get-field internals dealer1))

  ;(printf "curr_internals: ~a\n" curr-internals)

  ;; collect all cards that were distrubted by each hand, and make sure they are all unique
  (define (accul_internals curr-internals)
    (define accul_cards '())
    (for ([i curr-internals])
      (define p_cards (get-field my-cards (get-field player i)))
      (for ([j p_cards])
        (set! accul_cards (cons j accul_cards))
        )
    
      )
    (uniqueness accul_cards))
  (check-equal? (accul_internals curr-internals) #t)

  ;; variables meant to act as a track of the different add ons to the stack, both pushing and replacing under the two different circumstances
  (define fs1 '())
  (define fs2 '())
  (define fs3 '())
  (define fs4 '())

  (define sh1 1)
  (define sh2 1)
  (define sh3 1)
  (define sh4 1)
  ;; -----------------------------------------------------------------------------------------------
  (define (accul_internals-deck curr-internals deck)
    (define accul_cards '())
    (for ([i curr-internals])
      (define p_cards (get-field my-cards (get-field player i)))
      (for ([j p_cards])
        (set! accul_cards (cons j accul_cards))
        )
    
      )
    (for ([i (get-field my-stacks deck)])
      (for ([j i])
        (set! accul_cards (cons j accul_cards))
        )
      )
    
    (uniqueness accul_cards))
  ;; -----------------------------------------------------------------------------------------------
  (define (stack-alloc stack)
    (define alloced_cards '())
    (for ([i stack])
      (for ([j i])
        (set! alloced_cards (cons j alloced_cards))
        )
      )
    alloced_cards
    )
  ;; -----------------------------------------------------------------------------------------------
  ;; helper function to determine which stack has been chosen to be modified 
  (define (num_of_closest_fit stack closest_fit)
    (cond [(equal? closest_fit (first stack)) '(1 (length (first stack)))]
          [(equal? closest_fit (second stack)) '(2 (length (second stack)))]
          [(equal? closest_fit (third stack)) '(3 (length (third stack)))]
          [(equal? closest_fit (fourth stack)) '(4 (length (fourth stack)))]))
  ;; -----------------------------------------------------------------------------------------------
  ;; just-adding, take the fake stack array and add a push onto the correct one, then test accordingly
  (define (just_adding index leng stack)
    (cond
      [(equal? index 1)
       (set! sh1 (+ sh1 1))
       (set! fs1 (cons "push" fs1))
       (check-equal? (equal? (first stack) leng) #f)
       (check-equal? (length (first stack)) sh1)
       ]
      [(equal? index 2)
       (set! sh2 (+ sh2 1))
       (set! fs2 (cons "push" fs2))
       (check-equal? (equal? (second stack) leng) #f)
       (check-equal? (length (second stack)) sh2)
       ]
      [(equal? index 3)
       (set! sh3 (+ sh3 1))
       (set! fs3 (cons "push" fs3))
       (check-equal? (equal? (third stack) leng) #f)
       (check-equal? (length (third stack)) sh3)]
      [(equal? index 4)
       (set! sh4 (+ sh4 1))
       (set! fs4 (cons "push" fs4))
       (check-equal? (equal? (fourth stack) leng) #f)
       (check-equal? (length (fourth stack)) sh4)
       ])
    )  
  
  ;; -----------------------------------------------------------------------------------------------
  ;; just-replacing: checking that the fake stack is removing under correct circumstances and is modified accordingly
  (define (just-replacing index stack)
    (cond
      [(equal? index 1)
       (check-equal? fs1 '("push" "push" "push" "push"))
       (set! fs1 '())
       (set! sh1 1)
       (check-equal? (length (first stack)) sh1)
       ]
      [(equal? index 2)
       (check-equal? fs2 '("push" "push" "push" "push"))
       (set! fs2 '())
       (set! sh2 1)
       (check-equal? (length (second stack)) sh2)
       ]
      [(equal? index 3)
       (check-equal? fs3 '("push" "push" "push" "push"))
       (set! fs3 '())
       (set! sh3 1)
       (check-equal? (length (third stack)) sh3)
       ]
      [(equal? index 4)
       (check-equal? fs4 '("push" "push" "push" "push"))
       (set! fs4 '())
       (set! sh4 1)
       (check-equal? (length (fourth stack)) sh4)
       ]
      ))
  ;; -----------------------------------------------------------------------------------------------
  ;; choosen-to-replace: triggered when the player has a card that is less than all the rows, and must replace all the rows
  (define (choosen-to-replace index stack)
    (cond [(equal? index 1)
           (set! fs1 '())
           (set! sh1 1)
           (check-equal? (length (first stack)) sh1)
           ]
          [(equal? index 2)
           (set! fs2 '())
           (set! sh2 1)
           
           (check-equal? (length (second stack)) sh2)
           ]
          [(equal? index 3)
           (set! fs3 '())
           (set! sh3 1)
           
           (check-equal? (length (third stack)) sh3)
           ]
          [(equal? index 4)
           (set! fs4 '())
           (set! sh4 1)
           
           (check-equal? (length (fourth stack)) sh4)
           ]
          ))
  ;; -----------------------------------------------------------------------------------------------
  
  (define (added-bulls stack)
    (define bulls_ret 0)
    (define (helper stack)
    (cond [(equal? stack '())
           bulls_ret]
          [else
           (define curr_bull  (card-bulls (first stack)))
           (set! bulls_ret (+ bulls_ret curr_bull))
           (helper (rest stack))
           ]))
    (helper stack))
  ;; -----------------------------------------------------------------------------------------------
  (define (play-turn-loc deck)
      (define played-cards
        (for/list
                  ((p  (in-list (get-field internals dealer1))))
          (list p (send p start-turn deck))))
      (define sorted-played-cards
        (sort played-cards < #:key (lambda (x) (card-face (second x)))))
      (place-cards-loc deck sorted-played-cards))

    (define (place-cards-loc deck sorted-player-cards)
      (check-equal? (length (first (get-field my-stacks deck))) sh1)
      (check-equal? (length (second (get-field my-stacks deck))) sh2)
      (check-equal? (length (third (get-field my-stacks deck))) sh3)
      (check-equal? (length (fourth (get-field my-stacks deck))) sh4)
      (check-equal? (length fs1) (- sh1 1))
      (check-equal? (length fs2) (- sh2 1))
      (check-equal? (length fs3) (- sh3 1))
      (check-equal? (length fs4) (- sh4 1))
      (for ((p+c  (in-list sorted-player-cards)))
        (define player (first p+c))
        (define card (second p+c))
        (cond
          [(send deck larger-than-some-top-of-stacks? card)
           (define closest-fit-stack (send deck fit card))
           (define return-list (num_of_closest_fit (get-field my-stacks deck) closest-fit-stack))
           (define index (first return-list))
           (define curr_length (second return-list))
           
           (define pre-alloc-stack (stack-alloc (get-field my-stacks deck)))
           
           (cond
             [(< (length closest-fit-stack) FIVE)
              (define pre-bulls (send player bulls))
              (send deck push card)

              ;;
              (just_adding index curr_length (get-field my-stacks deck))
              ;(just-adding-checker index curr_length (get-field my-stacks deck))
              ;; stack
              (define post-alloc-stack (stack-alloc (get-field my-stacks deck)))
              ;; check that the stack was modified accordingly via a call the push
              (check-equal? (+ 1 (length pre-alloc-stack)) (length post-alloc-stack))
              ;; check that the bulls score did not change
              (define post-bulls (send player bulls))
              (check-equal? pre-bulls post-bulls)
              ]
             [(= (length closest-fit-stack) FIVE)
              
              (define pre-bulls (send player bulls))
              (define bulls (send deck replace closest-fit-stack card))
              (define post-alloc-stack (stack-alloc (get-field my-stacks deck)))
              ;; check that the stack was modified accordingly via a call to replace
              (check-equal? (length pre-alloc-stack) (+ 4 (length post-alloc-stack)))
              (just-replacing index (get-field my-stacks deck))
              (define add_bulls (added-bulls closest-fit-stack))
              (send player add-score bulls)
              ;; check that the bulls score is more than it was before
              (define post-bulls (send player bulls))
              (check-equal? (< pre-bulls post-bulls) #t)
              (check-equal? (+ pre-bulls add_bulls) post-bulls)
              ])]
          [else ;; the tops of all stacks have larger face values than card
           (define pre-bulls (send player bulls))
           (define chosen-stack (send player choose deck))
           (define add-bulls (added-bulls chosen-stack))
           (define return-lists (num_of_closest_fit (get-field my-stacks deck) chosen-stack))
           (define i (first return-lists))
           ;(define l (second return-lists))
           (define bulls (send deck replace chosen-stack card))
           (choosen-to-replace i (get-field my-stacks deck))
           (send player add-score bulls)
           ;; check that the bulls score is more than it was before
           (define post-bulls (send player bulls))
           (check-equal? (+ pre-bulls add-bulls) post-bulls)
           (check-equal? (< pre-bulls post-bulls) #t)])))
  ;;as the hands are played, check that they remain unique, (both before the hands and after)
  (for ((p HAND))
    (define pre_internals (get-field internals dealer1))
    (check-equal? (accul_internals-deck pre_internals deck) #t)
    (play-turn-loc deck)
    (define curr_internals (get-field internals dealer1))
    (check-equal? (accul_internals-deck curr_internals deck) #t)
    
    )




  
  )
