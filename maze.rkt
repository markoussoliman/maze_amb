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

(define assert-special
  (lambda (pred items)
    (if (not pred) (begin (set! items (cdr items)) (amb)))))

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

(an-element-of '((0 3)))

(define (another-element items)
  (assert (not (null? items)))
  (amb (list-ref items 0) (another-element items)))

(define (integer-small n)
  (amb n (integer-small (+ n 1))))


(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))


; example

(define (a-pythagorean-triple-between low high)
  (let ((i (number-between low high)))
    (let ((j (number-between i high)))
      (let ((k (number-between j high)))
        (assert (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define maze
 '((1 1 1 1 1 0 1)(1 0 2 0 1 0 1)(1 0 1 1 1 2 1)(1 0 2 0 0 0 1)(1 1 1 0 1 1 1)(1 0 2 0 2 0 1)(1 1 1 1 1 0 1))
)


(list-ref (list-ref maze 6) 1)

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

(define f (make-counter))



(define top-border (car maze))
(define begin-point
  (lambda (lst)
    (cond ((eq? 0 (list-ref lst (- (f) 1))) (list 0 (- (f) 2)))
          (else (begin-point lst)))))

;(begin-point top-border)

(define make-new-position
  (lambda (position direction)
    (cond ((eq? direction 'right) (list (car position) (+ 1 (cadr position))))
          ((eq? direction 'left) (list (car position) (- (cadr position) 1)))
          ((eq? direction 'forward) (list (+ 1 (car position)) (cadr position)))
          (else (display 'Wrong shit)))))

(make-new-position '(0 1) 'forward)


(define position-value
  (lambda (position)
    (list-ref (list-ref maze (car position)) (cadr position))))


(position-value '(0 6))

(define same-position?
  (lambda(position1 position2)
    (cond ((equal? position1 position2) #t)
          (else #f))))

(define exhausted-all?
  (lambda(items)
    (cond ((> (length items) 3) #t)
          (else #f))))

(define (list-with lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (list-with (cdr lst) (- idx 1) val))))


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
  

(define (maze-game)
  (define current-position '(0 0))
  (define previous-position '(0 0))
  (define end-point '(6 5))
  (define options '())
  (define b (begin-point top-border))
  (set! current-position b)
  (define lst (list b previous-position))
;  (display current-position)
  (let ((c (integer-small 1)))
    (display c)4
    (set! current-position (car lst))
    (set! previous-position (cadr lst))
    (let ((next-move (an-element-of '(forward right left))))
      (let ((next-move-position (make-new-position current-position next-move)))
    ;  (display current-position)
        (set! options (cons next-move options))
        (display options)
        (newline)
        (display lst)
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
              (set! lst (cdr lst))
              (set! maze (replace-maze maze current-position 1))
              (display maze)
              (newline)
              (set! options '())))

  ;      (assert-special (not (exhausted-all? options)) lst) 
        (assert (not (= (position-value next-move-position) 1)))
        (assert (not (same-position? next-move-position previous-position)))
        (set! lst (append (list next-move-position) lst))
        (set! previous-position current-position)
        (set! current-position next-move-position)
        (set! options '())
      ))
    (assert (equal? current-position end-point))
    (display 'DONE)))

(maze-game)