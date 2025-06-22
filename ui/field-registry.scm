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

(define FIELD-TYPE-TEXT 'text)
(define FIELD-TYPE-BOOLEAN 'boolean)
(define FIELD-SPACING 3) ; Lines between fields - just enough for boxes with no gap

(struct field
        (id ; Symbol identifier (e.g., 'search, 'replace)
         label ; Display label
         type ; Field type (text or boolean)
         default-value ; Default value
         row) ; Row position in the UI (0-based)
  #:transparent)

(define (get-all-fields)
  (define field-definitions
    (list (list 'search "Search pattern:" FIELD-TYPE-TEXT "")
          (list 'replace "Replace with:" FIELD-TYPE-TEXT "")
          (list 'fixed-strings "Fixed strings" FIELD-TYPE-BOOLEAN #f)
          (list 'match-whole-word "Match whole word" FIELD-TYPE-BOOLEAN #f)
          (list 'match-case "Match case" FIELD-TYPE-BOOLEAN #f)
          (list 'files-include "Files to include:" FIELD-TYPE-TEXT "")
          (list 'files-exclude "Files to exclude:" FIELD-TYPE-TEXT "")))

  ;; Map over the field definitions with their indices
  (let loop ([definitions field-definitions]
             [index 0]
             [result '()])
    (if (null? definitions)
        (reverse result)
        (loop (cdr definitions)
              (+ index 1)
              (cons (apply field (append (car definitions) (list index))) result)))))

(define (get-field-by-id id)
  (define matching-fields (filter (lambda (f) (equal? (field-id f) id)) (get-all-fields)))
  (if (null? matching-fields)
      #f
      (car matching-fields)))

;; Calculate Y positions for fields with no gaps between boxes
;; Returns a hash of field-id -> (label-y . value-y)
(define (calculate-field-positions content-y)
  ;; Use a running y-position to ensure no gaps between fields
  (let loop ([fields (get-all-fields)]
             [current-y content-y]
             [positions (hash)])
    (if (null? fields)
        positions
        (let* ([field (car fields)]
               [field-type (field-type field)]
               [field-height 3] ; All field types (text boxes and checkboxes) are 3 rows high
               [next-y (+ current-y field-height)])
          (loop (cdr fields)
                next-y
                (hash-insert positions (field-id field) (cons current-y current-y)))))))
