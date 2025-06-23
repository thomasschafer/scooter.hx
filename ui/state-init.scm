(require "fields.scm")

(provide create-initial-field-values
         create-initial-cursor-positions)

(define (create-initial-field-values)
  (fold (lambda (field-def values)
          (hash-insert values (field-id field-def) (field-default-value field-def)))
        (hash)
        (get-all-fields)))

(define (create-initial-cursor-positions)
  (fold (lambda (field-def positions)
          (if (equal? (field-type field-def) FIELD-TYPE-TEXT)
              (hash-insert positions (field-id field-def) 0)
              positions))
        (hash)
        (get-all-fields)))
