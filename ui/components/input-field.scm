;; Input field utilities for managing dual search/replace fields
(require-builtin helix/components)

(provide field-label
         field-position
         field-label-position
         toggle-field
         FIELD-SPACING)

;; UI Constants for field spacing
(define FIELD-SPACING 3)

;; Generate a field label with active indicator
(define (field-label field active?)
  (define prefix (if active? "> " "  "))
  (define label (if (equal? field 'search) "Search pattern:" "Replace with:"))
  (string-append prefix label))

;; Get the Y position for a field's input text
(define (field-position field content-y)
  (if (equal? field 'search)
      (+ content-y 1)
      (+ content-y 4)))

;; Get the Y position for a field's label
(define (field-label-position field content-y)
  (if (equal? field 'search)
      content-y
      (+ content-y FIELD-SPACING)))

;; Toggle between search and replace fields
(define (toggle-field current-field)
  (if (equal? current-field 'search) 'replace 'search))
