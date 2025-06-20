;; Field utility functions
;; Helper functions for working with field values and validation
(require "field-registry.scm")
(require (only-in "field-registry.scm" FIELD-TYPE-TEXT FIELD-TYPE-BOOLEAN))

(provide field-is-text?
         field-is-boolean?
         is-valid-field-id?
         get-text-field-ids
         get-boolean-field-ids)

;; Check if a field is a text field
(define (field-is-text? field-id)
  (define field-def (get-field-by-id field-id))
  (and field-def (equal? (field-type field-def) FIELD-TYPE-TEXT)))

;; Check if a field is a boolean field
(define (field-is-boolean? field-id)
  (define field-def (get-field-by-id field-id))
  (and field-def (equal? (field-type field-def) FIELD-TYPE-BOOLEAN)))

;; Check if a field ID is valid
(define (is-valid-field-id? field-id)
  (not (equal? (get-field-by-id field-id) #f)))

;; Get all text field IDs
(define (get-text-field-ids)
  (map field-id (filter (lambda (f) (equal? (field-type f) FIELD-TYPE-TEXT)) (get-all-fields))))

;; Get all boolean field IDs
(define (get-boolean-field-ids)
  (map field-id (filter (lambda (f) (equal? (field-type f) FIELD-TYPE-BOOLEAN)) (get-all-fields))))
