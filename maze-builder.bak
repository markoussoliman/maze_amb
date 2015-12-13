; build the maze
(define build-single-row
  (lambda(n last-element boundary even-val odd-val)
    (if (= n 0)
        '()
        (cons
         (cond ((or (= n 1) (= n last-element)) boundary)
               ((odd? n) odd-val)
               (else even-val))
         (build-single-row (- n 1) last-element boundary even-val odd-val)))))
               


(define process-maze
  (lambda(m n last-element)
    (if (= n 0)
        '()
        (cons
         (cond ((or (= n 1) (= n last-element)) (build-single-row m m 1 1 1))
               ((odd? n) (build-single-row m m 1 2 2))
               (else (build-single-row m m 1 0 2)))
         (process-maze m (- n 1) n)))))

(define maze-builder
  (lambda (m n)
    (let ((maze-m (+ (* m 2) 1))
          (maze-n (+ (* n 2) 1)))
      (process-maze maze-m maze-n maze-n))))

(maze-builder 4 4)
             


                   