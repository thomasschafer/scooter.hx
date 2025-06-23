(require "helix/components.scm")
(require "helix/configuration.scm")

(require "border.scm")
(require "field-registry.scm")
(require "field-utils.scm")
(require "utils.scm")

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

(define (get-field-styles active? text-style active-style)
  (let* ([base-style (if active? active-style text-style)]
         [title-style (if active?
                          (style-with-bold base-style)
                          base-style)])
    (list base-style title-style text-style)))

(define (draw-boolean-field frame
                            content-x
                            label-y
                            field-def
                            field-value
                            active?
                            text-style
                            active-style)
  (let* ([title (field-label field-def)]
         [checkbox-content (if field-value "X" " ")]
         [horizontal-line (make-string (- CHECKBOX-WIDTH 2) (string-ref BORDER-HORIZONTAL 0))]
         [checkbox-top (string-append BORDER-TOP-LEFT horizontal-line BORDER-TOP-RIGHT)]
         [checkbox-middle (string-append BORDER-VERTICAL " " checkbox-content " " BORDER-VERTICAL)]
         [checkbox-bottom (string-append BORDER-BOTTOM-LEFT horizontal-line BORDER-BOTTOM-RIGHT)]
         [field-styles (get-field-styles active? text-style active-style)]
         [box-style (car field-styles)]
         [title-style (cadr field-styles)])

    (frame-set-string! frame (+ content-x FIELD-PADDING-HORIZONTAL) label-y checkbox-top box-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 1)
                       checkbox-middle
                       box-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 2)
                       checkbox-bottom
                       box-style)

    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL CHECKBOX-WIDTH CHECKBOX-TEXT-GAP)
                       (+ label-y 1)
                       title
                       title-style)))

(define (draw-text-field-box frame
                             content-x
                             label-y
                             content-width
                             field-def
                             field-value
                             active?
                             text-style
                             active-style)
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

         [field-styles (get-field-styles active? text-style active-style)]
         [base-style (car field-styles)]
         [title-style (cadr field-styles)]
         [value-style (caddr field-styles)])

    (frame-set-string! frame (+ content-x FIELD-PADDING-HORIZONTAL) label-y box-top title-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 1)
                       BORDER-VERTICAL
                       base-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL (- box-width 1))
                       (+ label-y 1)
                       BORDER-VERTICAL
                       base-style)
    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL)
                       (+ label-y 2)
                       box-bottom
                       base-style)

    (frame-set-string! frame
                       (+ content-x FIELD-PADDING-HORIZONTAL CURSOR-PADDING)
                       (+ label-y 1)
                       (truncate-string (or field-value "") (- box-width (+ CURSOR-PADDING 2)))
                       value-style)))

(define (draw-field frame
                    content-x
                    content-width
                    field-def
                    field-value
                    active?
                    field-y-pos
                    text-style
                    active-style)
  (let* ([field-id (field-id field-def)]
         [field-type (field-type field-def)])

    (if (equal? field-type FIELD-TYPE-BOOLEAN)
        (draw-boolean-field frame
                            content-x
                            field-y-pos
                            field-def
                            field-value
                            active?
                            text-style
                            active-style)
        (draw-text-field-box frame
                             content-x
                             field-y-pos
                             content-width
                             field-def
                             field-value
                             active?
                             text-style
                             active-style))))

(define (draw-all-fields frame
                         content-x
                         content-y
                         content-width
                         current-field
                         state
                         text-style
                         active-style
                         field-value-getter)
  (let ([field-positions (calculate-field-positions content-y)])
    (let process-fields ([fields (get-all-fields)])
      (when (not (null? fields))
        (let* ([field-def (car fields)]
               [field-id (field-id field-def)]
               [active? (equal? current-field field-id)]
               [y-pos (car (hash-ref field-positions field-id))]
               [field-value (field-value-getter state field-id)])

          (draw-field frame
                      content-x
                      content-width
                      field-def
                      field-value
                      active?
                      y-pos
                      text-style
                      active-style)

          (process-fields (cdr fields)))))))

(define (get-field-cursor-column content-x cursor-pos)
  (+ content-x FIELD-PADDING-HORIZONTAL CURSOR-PADDING cursor-pos))
