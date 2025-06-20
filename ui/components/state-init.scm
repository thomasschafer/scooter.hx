;; State initialization utilities
;; Handles creation of initial ScooterWindow state
(require "field-registry.scm")
(require "field-utils.scm")
(require (only-in "field-registry.scm" FIELD-TYPE-TEXT))

(provide create-initial-field-values
         create-initial-cursor-positions)

;; Create initial field values hash from field registry
(define (create-initial-field-values)
  (fold (lambda (field-def values)
          (hash-insert values (field-id field-def) (field-default-value field-def)))
        (hash)
        (get-all-fields)))

;; Create initial cursor positions hash (only for text fields)
(define (create-initial-cursor-positions)
  (fold (lambda (field-def positions)
          (if (equal? (field-type field-def) FIELD-TYPE-TEXT)
              (hash-insert positions (field-id field-def) 0)
              positions))
        (hash)
        (get-all-fields)))
