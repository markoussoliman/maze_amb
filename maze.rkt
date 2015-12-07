; AMB CONSTRUCTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require compatibility/defmacro)

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            (error "amb tree exhausted")))))


(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)
          
          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                             (lambda ()
                               (set! amb-fail +prev-amb-fail)
                               (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)
          
          (+prev-amb-fail))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
            (lambda (+k)
              (set! amb-fail (lambda () (+k #f)))
              (let ((+v ,e))
                (set! +results (cons +v +results))
                (+k #t))))
           (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))


(define (an-element-of items)
  (assert (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))





; support code for puzzle, presented below

(define (member? e s)
  (cond ((null? s) #f)
        ((eq? e (car s)) #t)
        (else (member? e (cdr s)))))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member? (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAZE SAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define maze '())


; 3x3 MAZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define 3-3-maze  '((1 1 1 1 1 0 1)(1 0 2 0 1 0 1)(1 0 1 1 1 2 1)(1 0 2 0 0 0 1)(1 1 1 0 1 1 1)(1 0 2 0 2 0 1)(1 1 1 1 1 0 1)))

; 5x5 MAZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 5-5-maze  '((1 0 1 1 1 1 1 1 1 1 1)(1 0 2 0 1 0 2 0 2 0 1)(1 1 1 2 1 2 1 1 1 2 1)
                    (1 0 2 0 1 0 2 0 1 0 1)(1 2 1 1 1 1 1 2 1 1 1)(1 0 1 0 2 0 2 0 2 0 1)
                    (1 2 1 2 1 1 1 2 1 2 1)(1 2 1 2 1 2 1 1 1 2 1)(1 2 1 2 1 2 1 1 1 2 1)
                    (1 0 2 0 1 0 2 0 1 0 1)(1 1 1 1 1 1 1 1 1 0 1)))

; 4x6 MAZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 4-6-maze  '((1 0 1 1 1 1 1 1 1)(1 0 2 0 1 0 2 0 1)(1 1 1 2 1 2 1 2 1)
                    (1 0 2 0 1 0 1 0 1)(1 2 1 2 1 2 1 2 1)(1 0 1 0 1 0 1 0 1)
                    (1 2 1 2 1 2 1 2 1)(1 0 1 0 1 0 1 0 1)(1 2 1 2 1 2 1 2 1)
                    (1 0 1 0 1 0 1 0 1)(1 2 1 1 1 2 1 2 1)(1 0 2 0 2 0 1 0 1)
                    (1 1 1 1 1 1 1 0 1)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integer-small n)
  (amb n (integer-small (+ n 1))))


(define position
  (lambda(element lst)
    (cond ((null? lst) -1)
          ((eq? element (car lst)) 0)
          (else (+ 1 (position element (cdr lst)))))))


(define (make-counter)
  (define count 0)
  (define (increment)
    (begin
      (set! count (+ count 1))
      count)) 
  increment)


; now (make-counter) returns a closure -- <closure count increment> -- and calling
; this closure should do the job

; we name the closure

(define counter1 (make-counter))
(define counter2 (make-counter))

(define (last lst)
    (cond ((null? (cdr lst)) (car lst))
          (else (last (cdr lst)))))



(define top-border
  (lambda(maze)
    (car maze)))

(define bottom-border
  (lambda(maze)
    (last maze)))


(define bottom-border-position
  (lambda(maze)
    (cond ((equal? (car maze) (bottom-border maze)) 0)
          (else (+ 1 (bottom-border-position (cdr maze)))))))
  


(define find-begin-point
  (lambda (lst)
    (cond ((eq? 0 (list-ref lst (- (counter1) 1))) (list 0 (- (counter1) 2)))
          (else (find-begin-point lst)))))

(define find-end-point
  (lambda (lst)
    (cond ((eq? 0 (list-ref lst (- (counter2) 1))) (list (bottom-border-position (cdr maze)) (- (counter2) 2)))
          (else (find-end-point lst)))))

;(begin-point top-border)


(define make-new-position
  (lambda (position direction)
    (cond ((eq? direction 'right) (list (car position) (+ 1 (cadr position))))
          ((eq? direction 'left) (list (car position) (- (cadr position) 1)))
          ((eq? direction 'forward) (list (+ 1  (car position)) (cadr position)))
          ((eq? direction 'backward) (list (- (car position) 1) (cadr position)))
          (else (display 'Wrong shit)))))

;(make-new-position '(0 1) 'forward)


(define position-value
  (lambda (position maze)
    (list-ref (list-ref maze (car position)) (cadr position))))



(define same-position?
  (lambda(position1 position2)
    (cond ((equal? position1 position2) #t)
          (else #f))))

(define exhausted-all?
  (lambda(items)
    (cond ((> (length items) 4) #t)
          (else #f))))



(define (one-row-list lst index value)
  (if (null? lst)
      lst
      (cons
       (if (zero? index)
           value
           (car lst))
       (one-row-list (cdr lst) (- index 1) value))))

(define (replace-maze maze position value)
  (cond ((null? maze) '())
        (else (if (zero? (car position))
                  (cons (one-row-list (car maze) (cadr position) value) (replace-maze (cdr maze) (list (- (car position) 1) (cadr position)) value))
                  (cons (car maze) (replace-maze (cdr maze) (list (- (car position) 1) (cadr position)) value))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INITIALIZATION FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-position '(0 0))
(define previous-position '(0 0))
(define options '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN MAZE FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maze-game maze)
  (define begin-point (find-begin-point (top-border maze)))
  (define end-point (find-end-point (bottom-border maze)))
  (set! current-position begin-point)
  (define correct-path (list current-position previous-position))
;  (display (cadr correct-path))
  (let ((c (integer-small 1)))
    (set! current-position (car correct-path))
    (set! previous-position (cadr correct-path))
    (let ((next-move (an-element-of '(forward right left backward))))
      (let ((next-move-position (make-new-position current-position next-move)))
        (set! options (cons next-move options))
        (display options)
        (newline)
        (display correct-path)
        (newline)
        (display 'Previous-position:)
        (display previous-position)
        (newline)
        (display 'Current-position:)
        (display current-position)
        (newline)
        (display 'Next-move:)
        (display next-move)
        (newline)
        (display '------------------)
        (newline)
        (if (exhausted-all? options)
            (begin
              (set! correct-path (cdr correct-path))
              (set! maze (replace-maze maze current-position 1))
             ; (display maze)
             ; (newline)
              (set! options '())))

        (assert (not (= (position-value next-move-position maze) 1)))
        (assert (not (same-position? next-move-position previous-position)))
        (set! correct-path (append (list next-move-position) correct-path))
        (set! previous-position current-position)
        (set! current-position next-move-position)
        (set! options '())))

    (assert (equal? current-position end-point))
    (display c)
    (newline)
    (display 'Correct-path:)
    (display correct-path)
    (newline)
    (display 'DONE)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CALLING MAZE FUNCTIONS WITH DIFFERENT SAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(set! maze 3-3-maze)
;(maze-game 3-3-maze)
;(newline)
;(display '--------------------------------------------------------------------------)

;(set! maze 5-5-maze)
;(maze-game 5-5-maze)
;(newline)
;(display '--------------------------------------------------------------------------)
;;
(set! maze 4-6-maze)
(maze-game 4-6-maze)
(newline)
(display '--------------------------------------------------------------------------)
