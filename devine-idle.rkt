#lang racket
(require 2htdp/universe 2htdp/image math/base test-engine/racket-tests racket/runtime-path
         (only-in racket/gui/base
                  get-file 
                  put-file
                  message-box
                  queue-callback
                  get-display-size))
                 ; play-sound))

;;plant: plant being used currently
;;state: symbol saying what state is is in, 'new, 'plant, or 'menu
;;buttons: list of buttons
;;x and y of click for current tool: SUN
;;background: list of strings for background color, the first one is the current color
(struct game (plant state buttons x y background) #:prefab)
;;name: symbol of name of button- also used as text written on it
;;x-pos and y-pos: position on menu screen of the top left corner of the button
;;clicked?: if it is activated or not
(struct button (name pos-x pos-y clicked?) #:prefab)
;;age: an age
;;shape: list of two dimentions of the orb
;;colors: list of pre-colors to make orb and leaf in that order
;;new-branches: list of branches and smbranches
;;old-branches: list of branches and smbranches already drawn
;;scale: scale of whole plant
;;frozen-pic: frozen pic of everything unscaled not new, or #f
(struct plant (age shape colors new-branches old-branches scale frozen-pic) #:prefab)
;;x and y: are coordinates for how to draw the end of the leaf
;;rotation is the degrees it was rotated from the normal image, which has the base in the top left corner
;;place x and place y: Coordinates of root of the branch
;;now-x and now-y are the coordinates of the end of the leaf at the present time- important while it is growing
(struct branch (x y rotation pos-x pos-y now-x now-y) #:prefab)
(struct smbranch (x y rotation pos-x pos-y now-x now-y pull1 pull2) #:prefab)
;;Time Passed
(struct age (hours minutes seconds) #:prefab)
;;like a color but with prefab instead of transparent
(struct pre-color (red green blue) #:prefab)

(define-values (WIDTH-unchanged HEIGHT-unchanged) (get-display-size))
(define WIDTH (- WIDTH-unchanged 100))
(define HEIGHT (- HEIGHT-unchanged 100))
(define BACKGROUND-COLORS (list "white" "blue" "red" "orange" "yellow" "green" "indigo" "black" "violet"))
(define-runtime-path click-sound "click.wav")
(define-runtime-path techno-click-sound "techno-click.wav")
(define-runtime-path branch-grow-sound "branch-grow.wav")
(define-runtime-path new-scale-sound "new-scale.wav")

(define (STARTING-PLANT shape colors)
  (plant
   (age 0 0 0)
   shape
   colors
   empty
   empty
   1
   (freeze
    (draw-plant (BASE-STARTING-PLANT shape colors) "white"))))

(define (BASE-STARTING-PLANT shape colors)
  (plant
   (age 0 0 0)
   shape
   colors
   empty
   empty
   1
   #f))

(define LOGO
  (overlay
   (beside
    (text "DE" 100 "indigo")
    (text "VINE" 100 "yellow")
    (text " IDLE" 100 "indigo"))
   (rectangle WIDTH (/ HEIGHT 10) "solid" "transparent")))

(define BUTTONS
  (list
   (button 'RESUME (- (* WIDTH 13/16) (/ WIDTH 8)) (- (* HEIGHT 4/10) (/ HEIGHT 20)) #f)
   (button 'NEW (- (* WIDTH 3/4) (/ WIDTH 8)) (- (* HEIGHT 2/10) (/ HEIGHT 20)) #f)
   (button 'SAVE (- (* WIDTH 13/16) (/ WIDTH 8)) (- (* HEIGHT 6/10) (/ HEIGHT 20)) #f)
   (button 'LOAD (- (* WIDTH 3/4) (/ WIDTH 8)) (- (* HEIGHT 8/10) (/ HEIGHT 20)) #f)
   (button 'TALL (- (* WIDTH 1/4) (/ WIDTH 8)) (- (* HEIGHT 2/10) (/ HEIGHT 20)) #f)
   (button 'NORMAL (- (* WIDTH 3/16) (/ WIDTH 8)) (- (* HEIGHT 4/10)  (/ HEIGHT 20)) #t)
   (button 'DENSE (- (* WIDTH 3/16) (/ WIDTH 8)) (- (* HEIGHT 6/10) (/ HEIGHT 20)) #f)
   (button 'CONTINUOUS (- (* WIDTH 1/4) (/ WIDTH 8)) (- (* HEIGHT 8/10) (/ HEIGHT 20)) #f)
   (button 'SUN (- (* WIDTH 4/8) (/ WIDTH 8)) (- (* HEIGHT 4/10) (/ HEIGHT 20)) #f)
   (button 'SOUND (- (* WIDTH 2/4) (/ WIDTH 8)) (- (* HEIGHT 6/10) (/ HEIGHT 20)) #t)))

(define (NORMAL-RAND-BRANCH s c x y t?)
  (branch
   (list-ref (list (random-integer 30 75) (random-integer 40 75)) (random-integer 0 2))
   (list-ref (list (random-integer 30 75) (random-integer 40 75)) (random-integer 0 2))
   (cond
     [t?
      (DIRECTED-ROTATION s c x y)]
     [(= (random-integer 0 6) 0)
      (random-integer -45 46)]
     [(= (random-integer 0 3) 0)
      (random-integer 45 225)]
     [else
      (random-integer 90 180)])
   (first c)
   (second c)
   0
   0))

(define (TALL-RAND-BRANCH s c x y t?)
  (branch
   (list-ref (list (random-integer 30 75) (random-integer 50 75) (random-integer 50 75)) (random-integer 0 3))
   (list-ref (list (random-integer 30 75) (random-integer 50 75) (random-integer 50 75)) (random-integer 0 3))
   (cond
     [t?
      (DIRECTED-ROTATION s c x y)]
     [(= (random-integer 0 7) 0)
      (random-integer -45 46)]
     [(= (random-integer 0 3) 0)
      (random-integer 45 225)]
     [else
      (random-integer 90 180)])
   (first c)
   (second c)
   0
   0))

(define (DENSE-RAND-BRANCH s c x y t?)
  (branch
   (random-integer 30 75)
   (random-integer 30 75)
   (cond
     [t?
      (DIRECTED-ROTATION s c x y)]
     [(= (random-integer 0 3) 0)
      (random-integer -45 46)]
     [(= (random-integer 0 3) 0)
      (random-integer 45 225)]
     [else
      (random-integer 90 180)])
   (first c)
   (second c)
   0
   0))

(define (CONTINUOUS-RAND-BRANCH s c x y t?)
  (branch
   (list-ref (list (random-integer 30 75) (random-integer 50 75)) (random-integer 0 2))
   (list-ref (list (random-integer 30 75) (random-integer 50 75)) (random-integer 0 2))
   (cond
     [t?
      (DIRECTED-ROTATION s c x y)]
     [(= (random-integer 0 7) 0)
      (random-integer 45 225)]
     [else
      (random-integer 0 361)])
   (first c)
   (second c)
   0
   0))

(define (DIRECTED-ROTATION s c x y)
  (+
   (/ (* (atan (- (- y (* (plant-scale s) (y-adjust s (second c)))))
               (- x (* (plant-scale s) (x-adjust s (first c)))))
         180)
      pi)
   45))

(define (NORMAL-RAND-COORDINATES b l)
  (define placement-list (list 1 1 9/10 9/10 8/10 8/10 7/10 6/10 5/10 4/10))
  (cond
    [(and (= (random-integer 0 3) 1) (> l 3))
     (random-on-this-branch (list-ref (list (first b) (first b) (first b) (second b) (third b)) (random-integer 0 5)) placement-list)]
    [else
     (random-on-this-branch (list-ref b (random-integer 0 l)) placement-list)]))

(define (TALL-RAND-COORDINATES b l)
  (define placement-list (list 1 1 1 9/10 9/10 9/10 8/10 8/10 7/10 5/10))
  (cond
    [(and (= (random-integer 0 2) 1) (> l 2))
     (random-on-this-branch (first b) placement-list)]
    [else
     (random-on-this-branch (list-ref b (random-integer 0 l)) placement-list)]))

(define (DENSE-RAND-COORDINATES b l)
  (define placement-list (list 1 9/10 9/10 8/10 7/10 6/10 5/10 4/10 3/10 2/10))
  (cond
    [(and (= (random-integer 0 7) 0) (> l 3))
     (random-on-this-branch (list-ref (list (first b) (first b) (first b) (second b) (third b)) (random-integer 0 5)) placement-list)]
    [else
     (random-on-this-branch (list-ref b (random-integer 0 l)) placement-list)]))

(define (CONTINUOUS-RAND-COORDINATES b l)
  (define placement-list (list 1 1 1 1 9/10 9/10 8/10 8/10 9/10 8/10))
  (random-on-this-branch (first b) placement-list))

;;BIG-BANG------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (start)
  (define shape 
    (list
     (random-integer 30 80)
     (random-integer 30 60)))
  (define colors (final-colors (random-colors)))
  (big-bang
   (game
    #f
    'menu
    BUTTONS
    (* WIDTH 1/2)
    20
    BACKGROUND-COLORS)
   (on-draw game-draw)
   (on-tick game-tick 2/20)
   (on-key game-key)
   (on-mouse game-mouse)
   (name "Devine Idle")))

(define (random-colors)
  (list
   (pre-color  (random-integer 0 225) (random-integer 0 225) (random-integer 0 225))
   (pre-color  (random-integer 0 225) (random-integer 0 225) (random-integer 0 225))
   (pre-color  (random-integer 0 225) (random-integer 0 225) (random-integer 0 225))))
    

(define (final-colors colors)
  (cond
    [(equal? (random-integer 0 2) 0)
     colors]
    [else
     (list
      (first colors)
      (first colors)
      (second colors))]))

;;ON-MOUSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (game-mouse g x y event)
  (cond
    [(equal? (game-state g) 'menu)
     (cond
       [(equal? event "button-down")
        (clicked-in-menu g x y)]
       [else g])]
    [(and (equal? event "button-down") (equal? (game-state g) 'plant) (button-clicked? (extract-button (game-buttons g) 'SUN)))
     (new-tool-coordinates g x y)]
    [else g]))

(define (new-tool-coordinates g x y)
  (game
   (game-plant g)
   'plant
   (game-buttons g)
   (max (min x (- WIDTH 20)) 20)
   (max (min y (- HEIGHT 20)) 20)
   (game-background g)))

;;game x y-> game
(define (clicked-in-menu g x y)
  (define sound? (button-clicked? (extract-button (game-buttons g) 'SOUND)))
  (cond
    [(equal? (button-clicked (game-buttons g) x y sound?) #f)
     g]
    [else
     (game
      (game-plant g)
      (game-state g)
      (buttons-with-one-clicked g (button-clicked (game-buttons g) x y sound?))
      (game-x g)
      (game-y g)
      (game-background g))]))

(define (button-clicked b x y sound?)
  (cond
    [(empty? b)
     #f]
    [(and
      (>= x (button-pos-x (first b)))
      (<= x (+ (/ WIDTH 4) (button-pos-x (first b))))
      (>= y (button-pos-y (first b)))
      (<= y (+ (/ HEIGHT 10) (button-pos-y (first b))))
      (not (equal? (button-name (first b)) 'NEW))
      sound?)
     (begin
       ;(play-sound click-sound #t)
       (first b))]
    [(and (>= x (button-pos-x (first b))) (<= x (+ (/ WIDTH 4) (button-pos-x (first b)))) (>= y (button-pos-y (first b))) (<= y (+ (/ HEIGHT 10) (button-pos-y (first b)))))
     (first b)]
    [else (button-clicked (rest b) x y sound?)]))

;;game, button-> list of buttons
(define (buttons-with-one-clicked g b)
  (define height-button? (or (equal? (button-name b) 'TALL) (equal? (button-name b) 'DENSE) (equal? (button-name b) 'NORMAL) (equal? (button-name b) 'CONTINUOUS)))
  (cond
    [(and (button-clicked? b) (not height-button?))
     (cons
      (button
       (button-name b)
       (button-pos-x b)
       (button-pos-y b)
       #f)
      (list-without-this-button (game-buttons g) (button-name b)))]
    [height-button?
     (cons
      (button
       (button-name b)
       (button-pos-x b)
       (button-pos-y b)
       #t)
      (list-without-this-button (turn-off-height-buttons (game-buttons g)) (button-name b)))]
    [else
     (cons
      (button
       (button-name b)
       (button-pos-x b)
       (button-pos-y b)
       #t)
      (list-without-this-button (game-buttons g) (button-name b)))]))

(define (turn-off-height-buttons b)
  (cond
    [(empty? b)
     empty]
    [(or (equal? (button-name (first b)) 'TALL) (equal? (button-name (first b)) 'DENSE) (equal? (button-name (first b)) 'NORMAL) (equal? (button-name (first b)) 'CONTINUOUS))
     (cons
      (button
       (button-name (first b))
       (button-pos-x (first b))
       (button-pos-y (first b))
       #f)
      (turn-off-height-buttons (rest b)))]
    [else
     (cons (first b) (turn-off-height-buttons (rest b)))]))

(define (list-without-this-button bs name)
  (cond
    [(empty? bs)
     empty]
    [(equal? name (button-name (first bs)))
     (rest bs)]
    [else
     (cons
      (first bs)
      (list-without-this-button (rest bs) name))]))

;;ON-KEY--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (game-key g key)
  (define b-game
    (game
     (game-plant g)
     (game-state g)
     (game-buttons g)
     (game-x g)
     (game-y g)
     (append
      (rest (game-background g))
      (list
       (first (game-background g))))))
  (define m-game
    (game
     (game-plant g)
     (cond
       [(equal? (game-state g) 'menu)
        'plant]
       [(equal? (game-state g) 'plant)
        'menu]
       [else (game-state g)])
     (game-buttons g)
     (game-x g)
     (game-y g)
     (game-background g)))
  (define not-beginning? (not (equal? (game-plant g) #f)))
  (define sound? (button-clicked? (extract-button (game-buttons g) 'SOUND)))
  (cond
    [(and (equal? key "b") sound?)
     (begin
      ; (play-sound click-sound #t)
       b-game)]
    [(equal? key "b")
     b-game]
    [(and (equal? key "m") sound? not-beginning?)
     (begin
       ;(play-sound click-sound #t)
       m-game)]
    [(and (equal? key "m") not-beginning?)
     m-game]
    [(and (equal? key " ") (equal? (game-state g) 'new) (< (age-seconds (plant-age (game-plant g))) 4))
     (cond
       [sound?
        (begin
          ;(play-sound techno-click-sound #t)
          (continue-intro g))]
       [else
        (continue-intro g)])]
    [(and (equal? key " ") (equal? (game-state g) 'new))
     (cond
       [sound?
        (begin
        ;  (play-sound new-scale-sound #t)
          (continue-intro g))]
       [else
        (continue-intro g)])]
    [else g]))

(define (continue-intro g)
  (define p (game-plant g))
  (cond
    [(= (+ 1 (age-seconds (plant-age p))) 5)
     (game
      p
      'plant
      (game-buttons g)
      (game-x g)
      (game-y g)
      (game-background g))]
    [else
     (game
      (plant
       (age
        0
        0
        (+ 1 (age-seconds (plant-age p))))
       (plant-shape p)
       (plant-colors p)
       empty
       empty
       1
       (plant-frozen-pic p))
      (game-state g)
      (game-buttons g)
      (game-x g)
      (game-y g)
      (game-background g))]))

;;ON-TICK-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (game-tick g)
  (cond
    [(equal? (game-state g) 'plant)
     (game
      (plant-tick
       (game-plant g)
       (current-height-setting (game-buttons g))
       (game-x g)
       (game-y g)
       (button-clicked? (extract-button (game-buttons g) 'SUN))
       (first (game-background g))
       (button-clicked? (extract-button (game-buttons g) 'SOUND)))
      'plant
      (game-buttons g)
      (game-x g)
      (game-y g)
      (game-background g))]
    [(equal? (game-state g) 'menu)
     (button-animation (menu-tick g))]
    [else g]))

(define (menu-tick g)
  (define resume-button (extract-button (game-buttons g) 'RESUME))
  (define save-button (extract-button (game-buttons g) 'SAVE))
  (define load-button (extract-button (game-buttons g) 'LOAD))
  (define new-button (extract-button (game-buttons g) 'NEW))
  (define new-button-game
    (game
     (STARTING-PLANT (list (random-integer 30 80) (random-integer 30 60)) (final-colors (random-colors)))
     'new
     (cons
      (button
       (button-name new-button)
       (button-pos-x new-button)
       (button-pos-y new-button)
       #f)
      (list-without-this-button (game-buttons g) new-button))
     (game-x g)
     (game-y g)
     (game-background g)))
  (cond
    [(and (button-clicked? resume-button) (not (equal? (game-plant g) #f)))
     (game
      (game-plant g)
      'plant
      (cons
       (button
        (button-name resume-button)
        (button-pos-x resume-button)
        (button-pos-y resume-button)
        #f)
       (list-without-this-button (game-buttons g) resume-button))
      (game-x g)
      (game-y g)
      (game-background g))]
    [(button-clicked? new-button)
     (cond
       [(button-clicked? (extract-button (game-buttons g) 'SOUND))
        (begin
          ;(play-sound techno-click-sound #t)
          new-button-game)]
       [else new-button-game])]
    [(button-clicked? save-button)
     (define name (put-file))
     (define reset-save-button (button-reset g 'menu save-button))
     (cond
       [(equal? name #f)
        reset-save-button]
       [else
        (begin
          (with-output-to-file name #:exists 'truncate
            (lambda ()
              (write (game-without-plant-pic reset-save-button))))
          reset-save-button)])]
    [(button-clicked? load-button)
     (define name (get-file))
     (cond
       [(equal? name #f)
        (button-reset g 'menu load-button)]
       [else
        (with-input-from-file name (lambda () (read)))])]
    [else g]))

(define (button-animation g)
  (define new-button (extract-button (game-buttons g) 'NEW))
  (cond
    [(equal? (game-plant g) #f)
     (game
      (game-plant g)
      (game-state g)
      (cons
       (animate-button new-button)
       (list-without-this-button (game-buttons g) 'NEW))
      (game-x g)
      (game-y g)
      (game-background g))]
    [else
     g]))

(define (animate-button b)
  (cond
    [(equal?
      (button-pos-y b)
      (button-pos-y (second BUTTONS)))
     (button
      'NEW
      (button-pos-x b)
      (+ 10 (button-pos-y b))
      (button-clicked? b))]
    [else
     (button
      'NEW
      (button-pos-x b)
      (- (button-pos-y b) 10)
      (button-clicked? b))]))

(define (game-without-plant-pic g)
  (define p (game-plant g))
  (game
   (plant
    (plant-age p)
    (plant-shape p)
    (plant-colors p)
    (plant-new-branches p)
    (plant-old-branches p)
    (plant-scale p)
    #f)
   (game-state g)
   (game-buttons g)
   (game-x g)
   (game-y g)
   (game-background g)))

;;game, state, button-> game
(define (button-reset g s b)
  (game
   (game-plant g)
   s
   (cons
    (button
     (button-name b)
     (button-pos-x b)
     (button-pos-y b)
     #f)
    (list-without-this-button (game-buttons g) b))
   (game-x g)
   (game-y g)
   (game-background g)))

(define (extract-button b n)
  (cond
    [(equal?
      n
      (button-name (first b)))
     (first b)]
    [else
     (extract-button (rest b) n)]))

(define (current-height-setting b)
  (cond
    [(button-clicked? (extract-button b 'TALL))
     'TALL]
    [(button-clicked? (extract-button b 'DENSE))
     'DENSE]
    [(button-clicked? (extract-button b 'NORMAL))
     'NORMAL]
    [(button-clicked? (extract-button b 'CONTINUOUS))
     'CONTINUOUS]))

;;ON-TICK- plant
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (plant-tick s h x y t? background-color sound?)
  (define b (still-growing-and-adult-branches (plant-new-branches s)))
  (cond
    [(empty? (second b))
     (resize
      (plant
       (new-age (plant-age s))
       (plant-shape s)
       (plant-colors s)
       (grow-animation (grow-branches-sometimes-all s (grow-branch? s sound?) (grow-smbranch? s) h x y t?))
       (plant-old-branches s)
       (plant-scale s)
       (plant-frozen-pic s))
      background-color
      sound?)]
    [else
      (plant-tick
       (plant
        (plant-age s)
        (plant-shape s)
        (plant-colors s)
        (first b)
        (append
         (second b)
         (plant-old-branches s))
        (plant-scale s)
        (new-freeze
         (plant
          (plant-age s)
          (plant-shape s)
          (plant-colors s)
          (first b)
          (append
           (second b)
           (plant-old-branches s))
          (plant-scale s)
          (plant-frozen-pic s))
         background-color))
       h x y t? background-color sound?)]))

(define (new-freeze s background-color)
  (freeze
   (scale (plant-scale s)
          (draw-plant
           (plant
            (plant-age s)
            (plant-shape s)
            (plant-colors s)
            (plant-new-branches s)
            (plant-old-branches s)
            (plant-scale s)
            #f)
           background-color))))

(define (resize s background-color sound?)
  (define (make-scaled freeze)
    (plant
     (plant-age s)
     (plant-shape s)
     (plant-colors s)
     (plant-new-branches s)
     (plant-old-branches s)
     (- (plant-scale s) (/ (plant-scale s) 4))
     freeze))
  (cond
    [(and (branch-touching-edge? s) sound?)
     (begin
       ;(play-sound new-scale-sound #t)
       (make-scaled (new-freeze (make-scaled #f) background-color)))]
    [(branch-touching-edge? s)
     (make-scaled (new-freeze (make-scaled #f) background-color))]
    [else s]))

(define (branch-touching-edge? s)
  (cond
    [(and (<= (image-width (draw-plant s "white")) WIDTH) (<= (image-height (draw-plant s "white")) HEIGHT))
     #f]
    [else #t]))

;state, whether to grow a new branch, whether to grow a new small branch or not-> list of new branches
(define (grow-branches-sometimes-all s b? smb? h x y t?)
  (cond
    [b?
     (cons
      (rand-branch s h x y t?)
      (grow-branches-sometimes-all s #f smb? h x y t?))]
    [smb?
     (cons
      (rand-smbranch s)
      (grow-branches-sometimes-all s #f #f h x y t?))]
    [else
     (plant-new-branches s)]))

;;state->list of branches
(define (grow-branch? state sound?)
  (define a (plant-age state))
  (define s (age-seconds a))
  (define m (age-minutes a))
  (define h (age-hours a))
  (cond
    [(integer? s)
     (cond
       [(or
         (and (= (remainder s 8) 0) (< m 1) (< h 1))
         (and (= (remainder s 10) 0) (= m 1) (< h 1))
         (and (= (remainder s 15) 0) (or (= m 3) (= m 2)) (< h 1))
         (and (= (remainder s 15) 0) (< m 30) (> m 3) (< h 1))
         (and (= (remainder s 20) 0) (> m 30) (< h 1))
         (and (= (remainder s 25) 0) (> h 1)))
        (cond
          [sound?
           (begin
            ; (play-sound branch-grow-sound #t)
             #t)]
          [else #t])]
       [else #f])]
    [else #f]))

(define (grow-smbranch? s)
  (cond
    [(and
      (integer? (age-seconds (plant-age s)))
      (equal?
       (random-integer
        0
        (chance-of-growing-smbranch? s))
       0))
     #t]
    [else #f]))

(define (chance-of-growing-smbranch? s)
  (cond
    [(and (< (age-minutes (plant-age s)) 1) (< (age-hours (plant-age s)) 1))
     5]
    [(and (> (age-minutes (plant-age s)) 1) (< (age-hours (plant-age s)) 1))
     7]
    [(> (age-hours (plant-age s)) 1)
     10]
    [else
     8]))

(define (rand-smbranch s)
  (define c (new-smbranch-coordinates s))
  (define p (random-pulls s))
  (define c-s (small-small-or-big-small-branch? s))
  (smbranch
   (first c-s)
   (second c-s)
   (random-integer -45 225)
   (first c)
   (second c)
   0
   0
   (first p)
   (second p)))

(define (small-small-or-big-small-branch? s)
  (cond
    [(= (random-integer 0 4) 0)
     (list
      (random-integer 25 70)
      (random-integer 25 70))]
    [else
     (list
      (random-integer 25 45)
      (random-integer 25 45))]))

(define (random-pulls s)
  (cond
    [(= (random-integer 0 3) 0)
     (list
      (random-integer -90 91)
      (random-integer -90 91))]
    [else
     (define random-squig (list-ref (list (random-integer 80 91) (random-integer -80 -91)) (random-integer 0 2)))
     (list random-squig random-squig)]))

;;list of branches-> list of a list of branches still growing and a list of branches that are done growing
(define (still-growing-and-adult-branches b)
  (list
   (append
    (still-growing-branches (extract-branches b))
    (still-growing-smbranches (extract-smbranches b)))
   (append
    (adult-branches (extract-branches b))
    (adult-smbranches (extract-smbranches b)))))

;;list of branches-> list of branches that are still growing
(define (still-growing-branches b)
  (cond
    [(empty? b)
     empty]
    [(branch-needs-animation? (first b))
     (cons
      (first b)
      (still-growing-branches (rest b)))]
    [else
     (still-growing-branches (rest b))]))

(define (still-growing-smbranches b)
  (cond
    [(empty? b)
     empty]
    [(smbranch-needs-animation? (first b))
     (cons
      (first b)
      (still-growing-smbranches (rest b)))]
    [else
     (still-growing-smbranches (rest b))]))

(define (adult-branches b)
  (cond
    [(empty? b)
     empty]
    [(branch-needs-animation? (first b))
     (adult-branches (rest b))]
    [else
     (cons
      (first b)
      (adult-branches (rest b)))]))

(define (adult-smbranches b)
  (cond
    [(empty? b)
     empty]
    [(smbranch-needs-animation? (first b))
     (adult-smbranches (rest b))]
    [else
     (cons
      (first b)
      (adult-smbranches (rest b)))]))

(define (branch-needs-animation? b)
  (cond
    [(empty? b)
     #f]
    [(and (= (branch-now-x b) (branch-x b)) (= (branch-now-y b) (branch-y b)))
     #f]
    [else #t]))

(define (smbranch-needs-animation? b)
  (cond
    [(empty? b)
     #f]
    [(and (= (smbranch-now-x b) (smbranch-x b)) (= (smbranch-now-y b) (smbranch-y b)))
     #f]
    [else #t]))

;;list of new-branches->list of new-branches
(define (grow-animation branches)
  (define b (extract-branches branches))
  (define smb (extract-smbranches branches))
  (append
   (cond
     [(empty? b)
      empty]
     [(and (= (branch-now-x (first b)) (branch-x (first b))) (= (branch-now-y (first b)) (branch-y (first b))))
      (cons (first b) (grow-animation (rest b)))]
     [else
      (cons
       (branch
        (branch-x (first b))
        (branch-y (first b))
        (branch-rotation (first b))
        (branch-pos-x (first b))
        (branch-pos-y (first b))
        (+ (branch-now-x (first b)) (/ (branch-x (first b)) 16))
        (+ (branch-now-y (first b)) (/ (branch-y (first b)) 16)))
       (grow-animation (rest b)))])
   (sm-grow-animation smb)))

;;list of smbranches->list of smbranches
(define (sm-grow-animation b)
  (cond
    [(empty? b)
     empty]
    [(and (= (smbranch-now-x (first b)) (smbranch-x (first b))) (= (smbranch-now-y (first b)) (smbranch-y (first b))))
     (cons (first b) (sm-grow-animation (rest b)))]
    [else
     (cons
      (smbranch
       (smbranch-x (first b))
       (smbranch-y (first b))
       (smbranch-rotation (first b))
       (smbranch-pos-x (first b))
       (smbranch-pos-y (first b))
       (+ (smbranch-now-x (first b)) (/ (smbranch-x (first b)) 16))
       (+ (smbranch-now-y (first b)) (/ (smbranch-y (first b)) 16))
       (smbranch-pull1 (first b))
       (smbranch-pull2 (first b)))
      (sm-grow-animation (rest b)))]))

(define (rand-branch s h x y t?)
  (define c (new-branch-coordinates s h))
  (cond
    [(equal? h 'TALL)
     (TALL-RAND-BRANCH s c x y t?)]
    [(equal? h 'DENSE)
     (DENSE-RAND-BRANCH s c x y t?)]
    [(equal? h 'NORMAL)
     (NORMAL-RAND-BRANCH s c x y t?)]
    [(equal? h 'CONTINUOUS)
     (CONTINUOUS-RAND-BRANCH s c x y t?)]))

(define (new-age a)
  (cond
    [(= (age-minutes a) 60)
     (new-age
      (age (+ 1 (age-hours a)) 0 (age-seconds a)))]
    [(>= (age-seconds a) 60)
     (new-age
      (age (age-hours a) (+ 1 (age-minutes a)) 0))]
    [else
     (age (age-hours a) (age-minutes a) (+ (age-seconds a) 2/20))]))

(check-expect
 (new-age (age 1 1 59))
 (age 1 1 61))
(check-expect
 (new-age (age 1 1 60))
 (age 1 2 0))
(check-expect
 (new-age (age 1 60 59))
 (age 2 0 61))
(check-expect
 (new-age (age 1 1 1))
 (age 1 1 3))

;;New-branch-coordinates
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (new-branch-coordinates s h)
  (define b
    (cond
      [(empty? (plant-new-branches s))
       (extract-branches (plant-old-branches s))]
      [else
       (append
        (extract-branches (plant-new-branches s))
        (extract-branches (plant-old-branches s)))]))
  (define l (length b))
  (cond
    [(empty? b)
     (list (/ WIDTH 2) (- (- HEIGHT 90) (/ HEIGHT 6)))]
    [(equal? h 'CONTINUOUS)
        (CONTINUOUS-RAND-COORDINATES b l)]
    [(and (< (age-minutes (plant-age s)) 1) (= (random-integer 0 4) 0))
     (list
      (list-ref (list (- (/ WIDTH 2) 20) (+ 20 (/ WIDTH 2))) (random-integer 0 2))
      (- (- HEIGHT 80) (/ HEIGHT 6)))]
    [else
     (cond
       [(equal? h 'NORMAL)
        (NORMAL-RAND-COORDINATES b l)]
       [(equal? h 'TALL)
        (TALL-RAND-COORDINATES b l)]
       [(equal? h 'DENSE)
        (DENSE-RAND-COORDINATES b l)])]))

;; branch and list of ten fractions
(define (random-on-this-branch b l)
  (define rp
    (rotate-point
     (make-rectangular (branch-x b) (branch-y b)) (branch-rotation b) l))
  (list
   (+ (branch-pos-x b) (real-part rp))
   (+ (branch-pos-y b) (imag-part rp))))

(define (rotate-point p deg list)
  (make-polar 
   (* (magnitude p) (list-ref list (random-integer 0 10)))
   (- (angle p) (* deg (/ pi 180)))))

(define (new-smbranch-coordinates s)
  (define b
    (cond
      [(empty? (plant-new-branches s))
       (extract-branches (plant-old-branches s))]
      [else
       (append
        (extract-branches (plant-new-branches s))
        (extract-branches (plant-old-branches s)))]))
  (define l (length b))
  (cond
    [(= l 0)
     (list (/ WIDTH 2) (- (- HEIGHT 90) (/ HEIGHT 6)))]
    [(and (= (random-integer 0 3) 0) (> l 2))
     (random-on-this-branch (list-ref (list (first b) (first b) (second b)) (random-integer 0 3)) '(1 9/10 8/10 7/10 6/10 6/10 5/10 4/10 3/10 2/10))]
    [else
     (random-on-this-branch (list-ref b (random-integer 0 l)) '(1 9/10 8/10 7/10 6/10 6/10 5/10 4/10 3/10 2/10))]))

;;ON-DRAW-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (game-draw g)
  (cond
    [(and (equal? (game-state g) 'plant) (button-clicked? (extract-button (game-buttons g) 'SUN)))
     (overlay/xy
      (radial-star 10 8 20 "solid" "yellow")
      (- (- (game-x g) 20))
      (- (- (game-y g) 20))
      (draw-plant (game-plant g) (first (game-background g))))]
    [(equal? (game-state g) 'plant)
     (draw-plant (game-plant g) (first (game-background g)))]
    [(equal? (game-state g) 'menu)
     (draw-menu g)]
    [(equal? (game-state g) 'new)
     (draw-new g)]
    [else
     (empty-scene 200 200)]))

(define (draw-menu g)
  (overlay/xy
   LOGO
   0 0
   (add-menu-text
    (add-buttons
     (game-buttons g)
     (empty-scene WIDTH HEIGHT (first (game-background g)))))))

(define (add-buttons b pic)
  (cond
    [(empty? b)
     pic]
    [else
     (overlay/xy
      (cond
        [(button-clicked? (first b))
         (overlay
          (rectangle  (/ WIDTH 4) (/ HEIGHT 10) "outline" "black")
          (text (symbol->string (button-name (first b))) 40 "indigo")
          (rectangle  (/ WIDTH 4) (/ HEIGHT 10) "solid" "yellow"))]
        [else
         (overlay
          (rectangle  (/ WIDTH 4) (/ HEIGHT 10) "outline" "black")
          (text (symbol->string (button-name (first b))) 40 "yellow")
          (rectangle  (/ WIDTH 4) (/ HEIGHT 10) "solid" "indigo"))])
      (- (button-pos-x (first b)))
      (- (button-pos-y (first b)))
      (add-buttons (rest b) pic))]))

(define (add-menu-text pic)
  pic)

;;ON-DRAW- PLANT
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (draw-plant s background-color)
  (cond
    [(equal? s #f)
     (empty-scene WIDTH HEIGHT background-color)]
    [(equal? (plant-frozen-pic s) #f)
     (add-all-branches
      (plant-old-branches s)
      (base-of-plant-on-rectangle s)
      s)]
    [else
     (add-text
      s
      (overlay
       (scale (plant-scale s)
              (add-all-branches
               (plant-new-branches s)
               (scale (/ (plant-scale s))
                      (plant-frozen-pic s))
               s))
       (empty-scene WIDTH HEIGHT background-color)))]))

(define (add-text s pic)
  (overlay/xy
   (text-fit
    (text
     (string-append
      "This plant's age is "
      (number->string (age-hours (plant-age s)))
      " hours, "
      (number->string (age-minutes (plant-age s)))
      " minutes, and "
      (simple-seconds s)
      " seconds. Press m for menu, and b to change background color")
     30
     "indigo"))
   -5 -5
   pic))

(define (text-fit pic)
  (cond
    [(>= (image-width pic) WIDTH)
     (scale 0.75 pic)]
    [else
     pic]))

;;plant->string
(define (simple-seconds s)
  (define secs (number->string (floor (age-seconds (plant-age s)))))
  secs
  #;
  (cond
    [(= (string-length secs) 1)
     secs]
    [(< (age-seconds (plant-age s)) 10)
     (substring secs 0 1)]
    [else
     (substring secs 0 2)]))

(define (add-all-branches b pic s)
  (cond
    [(empty? b) pic]
    [else
     (add-smbranches
      (extract-smbranches b)
      (add-branches
       (extract-branches b)
       pic
       s)
      s)]))

(define (extract-smbranches b)
  (cond
    [(empty? b)
     empty]
    [(smbranch? (first b))
     (cons
      (first b)
      (extract-smbranches (rest b)))]
    [else
     (extract-smbranches (rest b))]))

(define (extract-branches b)
  (cond
    [(empty? b)
     empty]
    [(branch? (first b))
     (cons
      (first b)
      (extract-branches (rest b)))]
    [else
     (extract-branches (rest b))]))

(define (add-branches b pic s)
  (cond
    [(empty? b)
     pic]
    [else
     (overlay/xy
      (crop/align
       "center"
       "center"
       200
       200
       (rotate
        (branch-rotation (first b))
        (one-branch (first b) (plant-colors s))))
      (- (x-adjust s (- (branch-pos-x (first b)) 100)))
      (- (y-adjust s (- (branch-pos-y (first b)) 100)))
      (add-branches (rest b) pic s))]))

;;branches pic and state-> pic
(define (add-smbranches b pic s)
  (cond
    [(empty? b)
     pic]
    [else
     (overlay/xy
      (crop/align
       "center" "center" 200 200
       (rotate
        (smbranch-rotation (first b))
        (one-smbranch (first b) (plant-colors s))))
      (- (x-adjust s (- (smbranch-pos-x (first b)) 105)))
      (- (y-adjust s (- (smbranch-pos-y (first b)) 105)))
      (add-smbranches (rest b) pic s))]))
  
(define (base-of-plant-on-rectangle s)
  (overlay/xy
   (base-of-plant s)
   (- (x-adjust s (- (/ WIDTH 2) 100)))
   (- (y-adjust s (- (- HEIGHT 100) (/ HEIGHT 6))))
   (rectangle (size-adjust s WIDTH) (size-adjust s HEIGHT) "solid" "transparent")))

(define (size-adjust s sz) (/ sz (plant-scale s)))
(define (x-adjust s x) (+ x (* 1/2 (- (size-adjust s WIDTH) WIDTH))))
(define (y-adjust s y) (+ y (* 1/2 (- (size-adjust s HEIGHT) HEIGHT))))

;;ON-DRAW- Base of plant
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (base-of-plant s)
  (add-curve 
   (half-of-base-of-plant s)
   ;;top point coordinates
   100
   10
   -100
   1/5
   ;;bottom point coordinates
   (- 100 (/ (first (plant-shape s)) 2))
   100
   -40
   1/3
   (make-pen (pre-color->color (second (plant-colors s))) 10 "solid" "round" "round")))

(define (half-of-base-of-plant s)
  (add-curve 
   (base-of-base-of-plant s)
   ;;top point coordinates
   100
   10
   -80
   1/5
   ;;bottom point coordinates
   (+ (/ (first (plant-shape s)) 2) 100)
   100 
   -140
   1/3
   (make-pen (pre-color->color (second (plant-colors s))) 10 "solid" "round" "round")))

(define (base-of-base-of-plant s)
  (overlay/xy
   (ellipse 5 10 "solid" "black")
   (-
    (-
     (+ 100 (/ (first (plant-shape s)) 2))
     (/ (first (plant-shape s)) 4)
     4))
   (- 50)
   (overlay/xy
    (ellipse 5 10 "solid" "black")
    (-
     (+ (/ (first (plant-shape s)) 4)
        (- 100 (/ (first (plant-shape s)) 2))))
    (- 50)
    (overlay
     (ellipse (first (plant-shape s)) (second (plant-shape s)) "solid" (pre-color->color (first (plant-colors s))))
     (rectangle 200 200 "solid" "transparent")))))

;;ON-DRAW- Branch
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;one branch-> pic
(define (one-branch b c)
  (add-curve
   (half-branch b c)
   100 100 50 -1/3
   (+ (branch-now-x b) 100) (+ (branch-now-y b) 100) -50 1/3
   (make-pen (pre-color->color (second c)) 5 "solid" "round" "round")))


(define (half-branch b c)
  (add-curve
   (overlay/xy
    (circle 10 "solid" (pre-color->color (first c)))
    (- 90) (- 90)
    (rectangle 200 200 "solid" "transparent"))
   100 100 50 1/3
   (+ 100 (branch-now-x b)) (+ (branch-now-y b) 100) -50 1/3
   (make-pen (pre-color->color (second c)) 5 "solid" "round" "round")))

;;ON-DRAW- Small Branch
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (one-smbranch b c)
  (add-curve
   (overlay/xy
    (circle 5 "solid" (pre-color->color (second c)))
    -95 -95
    (rectangle 200 200 "solid" "transparent"))
   100 100 (smbranch-pull1 b) 1
   (+ (smbranch-now-x b) 100) (+ 100 (smbranch-now-y b)) (smbranch-pull2 b) 1
   (make-pen (pre-color->color (third c)) 3 "solid" "round" "round")))

(define (pre-color->color p)
  (make-color
   (pre-color-red p) (pre-color-green p) (pre-color-blue p) 225))

;;NEW-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;ON-DRAW
(define (draw-new g)
  (define p (game-plant g))
  (cond
    [(equal? p #f)
     (draw-intro-scene 0)]
    [else
     (draw-intro-scene (age-seconds (plant-age p)))]))

(define (draw-intro-scene scene)
  (overlay
   (above
    (cond
      [(= scene 0)
       (text "This is a Devine plant" 60 "indigo")]
      [(= scene 1)
       (text "It will grow off of your computer's power" 60 "indigo")]
      [(= scene 2)
       (text "No two plants are ever the same" 60 "indigo")]
      [(= scene 3)
       (text "What will yours look like?" 60 "indigo")]
      [(= scene 4)
       (text "Are you ready??" 60 "indigo")]
      [else
       (rectangle 10 10 "solid" "transparent")])
    (text "PRESS SPACE TO CONTINUE" 20 "indigo"))
   (base-of-plant-on-rectangle
    (plant
     (age 0 0 0)
     '(60 55)
     (list
      (pre-color 100 0 255)
      (pre-color 255 255 0))
     empty
     empty
     1
     #f))
   (empty-scene WIDTH HEIGHT)))

(void
 (start))