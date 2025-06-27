(require "drawing.scm")
(require "styles.scm")
(require "utils.scm")

(provide draw-boolean-field
         draw-text-field-box
         draw-field
         draw-all-fields
         get-field-cursor-column
         get-all-fields
         get-field-by-id
         field
         field-is-text?
         field-is-boolean?
         is-valid-field-id?
         get-text-field-ids
         get-boolean-field-ids
         get-next-field
         get-previous-field
         field-id
         field-label
         field-type
         field-default-value
         field-row
         calculate-field-positions
         FIELD-TYPE-TEXT
         FIELD-TYPE-BOOLEAN
         FIELD-SPACING
         FIELD-PADDING-HORIZONTAL
         FIELD-PADDING-VERTICAL
         CHECKBOX-WIDTH
         CHECKBOX-TEXT-GAP
         CURSOR-PADDING
         MAX-FIELD-WIDTH)

(define FIELD-PADDING-HORIZONTAL 5)
(define FIELD-PADDING-VERTICAL 1)
(define CHECKBOX-WIDTH 5)
(define CHECKBOX-TEXT-GAP 1)
(define CURSOR-PADDING 2)
(define MAX-FIELD-WIDTH 150)

(define FIELD-TYPE-TEXT 'text)
(define FIELD-TYPE-BOOLEAN 'boolean)
(define FIELD-SPACING 3)

(struct field (id label type default-value row) #:transparent)

(define (field-is-text? field-id)
  (define field-def (get-field-by-id field-id))
  (and field-def (equal? (field-type field-def) FIELD-TYPE-TEXT)))

(define (field-is-boolean? field-id)
  (define field-def (get-field-by-id field-id))
  (and field-def (equal? (field-type field-def) FIELD-TYPE-BOOLEAN)))

(define (is-valid-field-id? field-id)
  (not (equal? (get-field-by-id field-id) #f)))

(define (get-text-field-ids)
  (map field-id (filter (lambda (f) (equal? (field-type f) FIELD-TYPE-TEXT)) (get-all-fields))))

(define (get-boolean-field-ids)
  (map field-id (filter (lambda (f) (equal? (field-type f) FIELD-TYPE-BOOLEAN)) (get-all-fields))))

(define (get-next-field current-field-id)
  (define field-ids (map field-id (get-all-fields)))
  (define current-index (index-of field-ids current-field-id))
  (define field-count (length field-ids))
  (if (and current-index (< current-index (- field-count 1)))
      (list-ref field-ids (+ current-index 1))
      (car field-ids)))

(define (get-previous-field current-field-id)
  (define field-ids (map field-id (get-all-fields)))
  (define current-index (index-of field-ids current-field-id))
  (if (and current-index (> current-index 0))
      (list-ref field-ids (- current-index 1))
      (list-ref field-ids (- (length field-ids) 1))))

(define (get-all-fields)
  (define field-definitions
    (list (list 'search "Search text" FIELD-TYPE-TEXT "")
          (list 'replace "Replace text" FIELD-TYPE-TEXT "")
          (list 'fixed-strings "Fixed strings" FIELD-TYPE-BOOLEAN #f)
          (list 'match-whole-word "Match whole word" FIELD-TYPE-BOOLEAN #f)
          (list 'match-case "Match case" FIELD-TYPE-BOOLEAN #t)
          (list 'files-include "Files to include" FIELD-TYPE-TEXT "")
          (list 'files-exclude "Files to exclude" FIELD-TYPE-TEXT "")))

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

(define (calculate-field-positions content-y)
  (let loop ([fields (get-all-fields)]
             [current-y content-y]
             [positions (hash)])
    (if (null? fields)
        positions
        (let* ([field (car fields)]
               [field-type (field-type field)]
               [field-height 3]
               [next-y (+ current-y field-height)])
          (loop (cdr fields)
                next-y
                (hash-insert positions (field-id field) (cons current-y current-y)))))))

(define (get-field-style active? ui-styles)
  (if active?
      (UIStyles-active ui-styles)
      (UIStyles-text ui-styles)))

(define (calculate-field-layout content-x content-width)
  (let* ([available-width (- content-width (* 2 FIELD-PADDING-HORIZONTAL))]
         [field-width (min available-width MAX-FIELD-WIDTH)]
         [offset (if (> available-width MAX-FIELD-WIDTH)
                     (quotient (- available-width MAX-FIELD-WIDTH) 2)
                     0)]
         [field-x (+ content-x FIELD-PADDING-HORIZONTAL offset)])
    (list field-x field-width)))

(define (draw-boolean-field frame
                            content-x
                            content-width
                            label-y
                            field-def
                            field-value
                            active?
                            ui-styles)
  (let* ([title (field-label field-def)]
         [checkbox-mark (if field-value "X" " ")]
         [field-style (get-field-style active? ui-styles)]
         [layout (calculate-field-layout content-x content-width)]
         [field-x (car layout)])

    (draw-box! frame
               field-x
               label-y
               CHECKBOX-WIDTH
               3
               field-style
               #:content (list (string-append " " checkbox-mark " ")))

    (draw-text-line! frame
                     (+ field-x CHECKBOX-WIDTH CHECKBOX-TEXT-GAP)
                     (+ label-y 1)
                     title
                     field-style)))

(define (draw-text-field-box frame
                             content-x
                             label-y
                             content-width
                             field-def
                             field-value
                             active?
                             ui-styles)
  (let* ([title (field-label field-def)]
         [field-style (get-field-style active? ui-styles)]
         [layout (calculate-field-layout content-x content-width)]
         [field-x (car layout)]
         [box-width (cadr layout)]
         [field-text (format-field-text (or field-value "") box-width)])

    (draw-box! frame
               field-x
               label-y
               box-width
               3
               field-style
               #:title title
               #:content (list field-text)
               #:padding 0)))

(define (format-field-text text box-width)
  (let* ([inner-width (- box-width 2)]
         [text-start-pos (- CURSOR-PADDING 1)]
         [text-available-width (- inner-width CURSOR-PADDING 1)]
         [truncated-text (truncate-string text text-available-width)]
         [text-length (string-length truncated-text)]
         [leading-spaces (make-space-string text-start-pos)]
         [trailing-spaces (make-space-string (- inner-width text-start-pos text-length))])
    (string-append leading-spaces truncated-text trailing-spaces)))

(define (draw-field frame content-x content-width field-def field-value active? field-y-pos ui-styles)
  (let* ([field-id (field-id field-def)]
         [field-type (field-type field-def)])

    (if (equal? field-type FIELD-TYPE-BOOLEAN)
        (draw-boolean-field frame
                            content-x
                            content-width
                            field-y-pos
                            field-def
                            field-value
                            active?
                            ui-styles)
        (draw-text-field-box frame
                             content-x
                             field-y-pos
                             content-width
                             field-def
                             field-value
                             active?
                             ui-styles))))

(define (draw-all-fields frame
                         content-x
                         content-y
                         content-width
                         current-field
                         state
                         ui-styles
                         field-value-getter)
  (let ([field-positions (calculate-field-positions content-y)])
    (let process-fields ([fields (get-all-fields)])
      (when (not (null? fields))
        (let* ([field-def (car fields)]
               [field-id (field-id field-def)]
               [active? (equal? current-field field-id)]
               [y-pos (car (hash-ref field-positions field-id))]
               [field-value (field-value-getter state field-id)])

          (draw-field frame content-x content-width field-def field-value active? y-pos ui-styles)

          (process-fields (cdr fields)))))))

(define (get-field-cursor-column content-x content-width cursor-pos)
  (let* ([layout (calculate-field-layout content-x content-width)]
         [field-x (car layout)])
    (+ field-x 1 (- CURSOR-PADDING 1) cursor-pos)))
