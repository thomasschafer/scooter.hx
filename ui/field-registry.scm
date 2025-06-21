;; Field registry system for managing all input fields
;; Provides a centralized definition of all search form fields
(require-builtin helix/components)

(provide get-all-fields
         get-field-by-id
         field-id
         field-label
         field-type
         field-default-value
         field-row
         calculate-field-positions
         FIELD-TYPE-TEXT
         FIELD-TYPE-BOOLEAN
         FIELD-SPACING)

;; Constants
(define FIELD-TYPE-TEXT 'text)
(define FIELD-TYPE-BOOLEAN 'boolean)
(define FIELD-SPACING 3) ; Lines between fields

;; Field structure
(struct field
        (id ; Symbol identifier (e.g., 'search, 'replace)
         label ; Display label
         type ; Field type (text or boolean)
         default-value ; Default value
         row) ; Row position in the UI (0-based)
  #:transparent)

;; Field registry - centralized field definitions
;; To add a new field, simply add it to this list
(define (get-all-fields)
  ;; Core search fields
  (list (field 'search
               "Search pattern:"
               FIELD-TYPE-TEXT
               ""
               0)
        (field 'replace
               "Replace with:"
               FIELD-TYPE-TEXT
               ""
               1)
        ;; Search options
        (field 'fixed-strings
               "Fixed strings"
               FIELD-TYPE-BOOLEAN
               #f
               2)
        (field 'match-whole-word
               "Match whole word"
               FIELD-TYPE-BOOLEAN
               #f
               3)
        (field 'match-case
               "Match case"
               FIELD-TYPE-BOOLEAN
               #f
               4)
        ;; File filtering
        (field 'files-include
               "Files to include:"
               FIELD-TYPE-TEXT
               ""
               5)
        (field 'files-exclude
               "Files to exclude:"
               FIELD-TYPE-TEXT
               ""
               6)))

;; Get a field by its ID
(define (get-field-by-id id)
  (define matching-fields (filter (lambda (f) (equal? (field-id f) id)) (get-all-fields)))
  (if (null? matching-fields)
      #f
      (car matching-fields)))

;; Calculate Y positions for fields
;; Returns a hash of field-id -> (label-y . value-y)
(define (calculate-field-positions content-y)
  (fold (lambda (f positions)
          (define row (field-row f))
          (define label-y (+ content-y (* row FIELD-SPACING)))
          (define value-y (+ label-y 1))
          (hash-insert positions (field-id f) (cons label-y value-y)))
        (hash)
        (get-all-fields)))
