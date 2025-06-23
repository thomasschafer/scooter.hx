(require-builtin helix/components)
(require "field-registry.scm")
(require "utils.scm")

(provide get-next-field
         get-previous-field)

(define field-ids (map field-id (get-all-fields)))

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
