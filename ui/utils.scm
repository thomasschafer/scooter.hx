(provide truncate-string
         take-right
         drop
         take-n
         index-of
         truncate-str-with-ellipsis
         char-width
         char-width-sum
         char-substring)

(define (truncate-string str max-width)
  (cond
    [(<= max-width 0) str]
    [(<= (char-width str) max-width) str]
    [else (let ([chars (string->list str)]) (list->string (take-n chars max-width)))]))

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

(define (truncate-str-with-ellipsis str max-width)
  (let* ([str-chars (string->list str)]
         [str-len (length str-chars)])
    (if (<= str-len max-width)
        str
        (let* ([ellipsis "…"]
               [available-width (- max-width (char-width ellipsis))])
          (if (<= available-width 0)
              ellipsis
              (string-append ellipsis (list->string (take-right str-chars available-width))))))))

(define (char-width str)
  (length (string->list str)))

(define (char-width-sum string-list)
  (apply + (map char-width string-list)))

(define (char-substring str start end)
  (let* ([chars (string->list str)]
         [len (length chars)])
    (cond
      [(>= start len) ""]
      [(>= start end) ""]
      [else
       (let ([actual-start (max 0 start)]
             [actual-end (min len end)])
         (list->string (take-n (drop chars actual-start) (- actual-end actual-start))))])))
