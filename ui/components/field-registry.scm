;; Field registry system for managing all input fields
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
         FIELD-TYPE-BOOLEAN)

;; Field type definitions
(define FIELD-TYPE-TEXT 'text)
(define FIELD-TYPE-BOOLEAN 'boolean)

;; Field structure
(struct field
        (id ; Symbol identifier (e.g., 'search, 'replace)
         label ; Display label
         type ; Field type (text or boolean)
         default-value ; Default value
         row) ; Row position in the UI (0-based)
  #:transparent)

;; Create the field registry with all fields
(define (get-all-fields)
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
  (define fields (get-all-fields))
  (define result (filter (lambda (f) (equal? (field-id f) id)) fields))
  (if (null? result)
      #f
      (car result)))

;; Calculate Y positions for fields
;; Returns a hash of field-id -> (label-y . value-y)
(define (calculate-field-positions content-y)
  (define fields (get-all-fields))

  ;; Build hash table by folding over the fields
  (fold (lambda (f positions)
          (define row (field-row f))
          (define label-y (+ content-y (* row 3)))
          (define value-y (+ label-y 1))
          (hash-insert positions (field-id f) (cons label-y value-y)))
        (hash)
        fields))
