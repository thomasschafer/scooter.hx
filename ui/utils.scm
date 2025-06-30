(provide truncate-string
         take-right
         drop
         take-n
         index-of)

(define (truncate-string str max-width)
  (if (and (> max-width 0) (> (string-length str) max-width))
      (substring str 0 max-width)
      str))

(define (take-right lst n)
  (define len (length lst))
  (if (<= len n)
      lst
      (drop lst (- len n))))

(define (drop lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

(define (take-n lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

(define (index-of lst elem)
  (let loop ([lst lst]
             [idx 0])
    (cond
      [(null? lst) #f]
      [(equal? (car lst) elem) idx]
      [else (loop (cdr lst) (+ idx 1))])))
