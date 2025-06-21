;; Field navigation utilities
;; Handles Tab/Shift+Tab navigation between fields
(require-builtin helix/components)
(require "field-registry.scm")

(provide get-next-field
         get-previous-field)

;; Cache field IDs for efficiency
(define field-ids (map field-id (get-all-fields)))

;; Helper to find index of element in list
(define (index-of lst elem)
  (let loop ([lst lst]
             [idx 0])
    (cond
      [(null? lst) #f]
      [(equal? (car lst) elem) idx]
      [else (loop (cdr lst) (+ idx 1))])))

;; Get the next field in tab order (wraps to first)
(define (get-next-field current-field-id)
  (define current-index (index-of field-ids current-field-id))
  (define field-count (length field-ids))
  (if (and current-index (< current-index (- field-count 1)))
      (list-ref field-ids (+ current-index 1))
      (car field-ids)))

;; Get the previous field in tab order (wraps to last)
(define (get-previous-field current-field-id)
  (define current-index (index-of field-ids current-field-id))
  (if (and current-index (> current-index 0))
      (list-ref field-ids (- current-index 1))
      (list-ref field-ids (- (length field-ids) 1))))
