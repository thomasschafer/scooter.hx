(#%require-dylib "libscooter_hx" (only-in unicode-display-width unicode-truncate-to-width))

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
  (unicode-truncate-to-width str max-width))

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
  (let ([str-width (char-width str)])
    (if (<= str-width max-width)
        str
        (let* ([ellipsis "…"]
               [ellipsis-width (char-width ellipsis)]
               [available-width (- max-width ellipsis-width)])
          (if (<= available-width 0)
              ellipsis
              ;; We need to keep the END of the string, not the beginning
              ;; So we need a more complex approach
              (let loop ([chars (reverse (string->list str))]
                         [width 0]
                         [result '()])
                (cond
                  [(null? chars) (string-append ellipsis (list->string result))]
                  [(>= width available-width) (string-append ellipsis (list->string result))]
                  [else
                   (let* ([ch (car chars)]
                          [ch-width (char-width (string ch))])
                     (if (> (+ width ch-width) available-width)
                         (string-append ellipsis (list->string result))
                         (loop (cdr chars) (+ width ch-width) (cons ch result))))])))))))

(define (char-width str)
  (unicode-display-width str))

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
