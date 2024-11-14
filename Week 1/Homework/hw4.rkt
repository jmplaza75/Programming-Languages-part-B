
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      empty))


(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))


(define (list-nth-mod xs n)
  (cond
    ((< n 0) (error "list-nth-mod: negative number"))
    ((null? xs) (error "list-nth-mod: empty list"))
    (else (list-ref xs (remainder n (length xs))))))


(define (stream-for-n-steps s n)
  (if (zero? n)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

(define funny-number-stream
  (letrec ([generate-stream (lambda (n)
                              (if (zero? (remainder n 5))
                                  (cons (* -1 n) (lambda () (generate-stream (+ n 1))))
                                  (cons n (lambda () (generate-stream (+ n 1))))))])
    (lambda () (generate-stream 1))))

(define dan-then-dog
    (letrec ([switch (lambda (img)
                (if (string=? img "dan.jpg")
                (cons "dan.jpg" (lambda () (switch "dog.jpg")))
                (cons "dog.jpg" (lambda () (switch "dan.jpg")))))])
    (lambda () (switch "dan.jpg"))))


(define (stream-add-zero s)
    (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists list1 list2)
    (letrec ([cycle (lambda (index1 index2) 
                (cons (cons  (list-nth-mod list1 index1) (list-nth-mod list2 index2)) 
                (lambda () (cycle (+ index1 1) (+ index2 1)))))])
    (lambda () (cycle 0 0))))


(define (vector-assoc v vec)
  (define (helper i)
    (cond
      ((>= i (vector-length vec)) #f) 
      ((pair? (vector-ref vec i))
       (let ([pair (vector-ref vec i)])
         (if (equal? v (car pair))
             pair
             (helper (+ i 1)))))
      (else (helper (+ i 1)))))
  (helper 0))


(define (cached-assoc xs n)
  (define cache (make-vector n #f)) ; Initialize the cache with #f

  (define (update-cache! pair)
    (vector-set! cache (modulo (vector-length cache) n) pair))

  (define (find-in-cache v)
    (let loop ((i 0))
      (if (= i n)
          #f
          (let ((cached-pair (vector-ref cache i)))
            (if (and cached-pair (equal? v (car cached-pair)))
                cached-pair
                (loop (+ i 1)))))))

  (define (assoc-with-cache v)
    (let ((cached-result (find-in-cache v)))
      (if cached-result
          cached-result
          (let ((result (assoc v xs)))
            (when result
              (update-cache! result))
            result))))

  assoc-with-cache) ; Return the function that uses the cache
