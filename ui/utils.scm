(provide truncate-string
         take-right
         drop
         take-n
         index-of)

(define (truncate-string str max-width)
  (let ([str-len (string-length str)])
    (cond
      [(<= max-width 0) str]
      [(<= str-len max-width) str]
      [else
       (let ([chars (string->list str)])
         (list->string (take-n chars max-width)))])))

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
