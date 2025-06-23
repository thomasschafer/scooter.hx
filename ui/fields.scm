(require "helix/components.scm")
(require "helix/configuration.scm")

(require "border.scm")
(require "field-registry.scm")
(require "field-utils.scm")
(require "utils.scm")
(require "styles.scm")

(provide FIELD-PADDING-HORIZONTAL
         FIELD-PADDING-VERTICAL
         draw-boolean-field
         draw-text-field-box
         draw-field
         draw-all-fields
         get-field-cursor-column)

(define FIELD-PADDING-HORIZONTAL 2)
(define FIELD-PADDING-VERTICAL 1)
(define CHECKBOX-WIDTH 5)
(define CHECKBOX-TEXT-GAP 1)
(define CURSOR-PADDING 2)

(define (get-field-style active? ui-styles)
  (if active?
      (UIStyles-active ui-styles)
      (UIStyles-text ui-styles)))

(define (draw-boolean-field frame content-x label-y field-def field-value active? ui-styles)
  (let* ([title (field-label field-def)]
         [checkbox-content (if field-value "X" " ")]
         [horizontal-line (make-string (- CHECKBOX-WIDTH 2) (string-ref BORDER-HORIZONTAL 0))]
         [checkbox-top (string-append BORDER-TOP-LEFT horizontal-line BORDER-TOP-RIGHT)]
         [checkbox-middle (string-append BORDER-VERTICAL " " checkbox-content " " BORDER-VERTICAL)]
         [checkbox-bottom (string-append BORDER-BOTTOM-LEFT horizontal-line BORDER-BOTTOM-RIGHT)]
         [field-style (get-field-style active? ui-styles)])

    (frame-set-string! frame (+ content-x FIELD-PADDING-HORIZONTAL) label-y checkbox-top field-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 1)
                       checkbox-middle
                       field-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 2)
                       checkbox-bottom
                       field-style)

    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL CHECKBOX-WIDTH CHECKBOX-TEXT-GAP)
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
  (let* ([box-width (- content-width (* 2 FIELD-PADDING-HORIZONTAL))]
         [title (field-label field-def)]
         [title-len (string-length title)]

         [title-start-pos 1]
         [right-border-pos (- box-width 1)]
         [remaining-border-width (- right-border-pos title-start-pos title-len)]

         [box-top (string-append BORDER-TOP-LEFT
                                 title
                                 (make-string remaining-border-width (string-ref BORDER-HORIZONTAL 0))
                                 BORDER-TOP-RIGHT)]
         [box-bottom (string-append BORDER-BOTTOM-LEFT
                                    (make-string (- box-width 2) (string-ref BORDER-HORIZONTAL 0))
                                    BORDER-BOTTOM-RIGHT)]

         [field-style (get-field-style active? ui-styles)])

    (frame-set-string! frame (+ content-x FIELD-PADDING-HORIZONTAL) label-y box-top field-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 1)
                       BORDER-VERTICAL
                       field-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL (- box-width 1))
                       (+ label-y 1)
                       BORDER-VERTICAL
                       field-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 2)
                       box-bottom
                       field-style)

    ;; Draw the complete inner line including leading/trailing spaces
    (let* ([inner-width (- box-width 2)]
           [text-start-pos (- CURSOR-PADDING 1)]
           [text-available-width (- inner-width CURSOR-PADDING 1)]
           [text-content (truncate-string (or field-value "") text-available-width)]
           [text-length (string-length text-content)]
           [leading-spaces (make-string text-start-pos #\space)]
           [trailing-spaces (make-string (- inner-width text-start-pos text-length) #\space)]
           [complete-line (string-append leading-spaces text-content trailing-spaces)])
      (frame-set-string! frame
                         (+ content-x FIELD-PADDING-HORIZONTAL 1)
                         (+ label-y 1)
                         complete-line
                         field-style))))

(define (draw-field frame content-x content-width field-def field-value active? field-y-pos ui-styles)
  (let* ([field-id (field-id field-def)]
         [field-type (field-type field-def)])

    (if (equal? field-type FIELD-TYPE-BOOLEAN)
        (draw-boolean-field frame content-x field-y-pos field-def field-value active? ui-styles)
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

(define (get-field-cursor-column content-x cursor-pos)
  (+ content-x FIELD-PADDING-HORIZONTAL CURSOR-PADDING cursor-pos))
