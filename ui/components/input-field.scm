;; Input field utilities
(require-builtin helix/components)
(require "field-registry.scm")

(provide get-next-field
         get-previous-field)

;; Get the next field in tab order
(define (get-next-field current-field-id)
  (define fields (get-all-fields))
  (define field-ids (map field-id fields))
  (define current-index (index-of field-ids current-field-id))
  (if (and current-index (< current-index (- (length field-ids) 1)))
      (list-ref field-ids (+ current-index 1))
      (car field-ids))) ; Wrap to first field

;; Get the previous field in tab order
(define (get-previous-field current-field-id)
  (define fields (get-all-fields))
  (define field-ids (map field-id fields))
  (define current-index (index-of field-ids current-field-id))
  (if (and current-index (> current-index 0))
      (list-ref field-ids (- current-index 1))
      (list-ref field-ids (- (length field-ids) 1)))) ; Wrap to last field

;; Helper to find index of element in list
(define (index-of lst elem)
  (let loop ([lst lst]
             [idx 0])
    (cond
      [(null? lst) #f]
      [(equal? (car lst) elem) idx]
      [else (loop (cdr lst) (+ idx 1))])))
