(require-builtin helix/components)
(require "field-registry.scm")

(provide get-next-field
         get-previous-field)

(define field-ids (map field-id (get-all-fields)))

(define (index-of lst elem)
  (let loop ([lst lst]
             [idx 0])
    (cond
      [(null? lst) #f]
      [(equal? (car lst) elem) idx]
      [else (loop (cdr lst) (+ idx 1))])))

(define (get-next-field current-field-id)
  (define current-index (index-of field-ids current-field-id))
  (define field-count (length field-ids))
  (if (and current-index (< current-index (- field-count 1)))
      (list-ref field-ids (+ current-index 1))
      (car field-ids)))

(define (get-previous-field current-field-id)
  (define current-index (index-of field-ids current-field-id))
  (if (and current-index (> current-index 0))
      (list-ref field-ids (- current-index 1))
      (list-ref field-ids (- (length field-ids) 1))))
