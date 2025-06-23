(require "helix/components.scm")

(provide draw-box!
         draw-text-line!
         draw-horizontal-line!
         draw-vertical-lines!
         make-border-string
         make-space-string
         draw-border!
         calculate-centered-layout
         CenteredLayout
         CenteredLayout-x
         CenteredLayout-y
         CenteredLayout-width
         CenteredLayout-height
         BORDER-TOP-LEFT
         BORDER-TOP-RIGHT
         BORDER-BOTTOM-LEFT
         BORDER-BOTTOM-RIGHT
         BORDER-HORIZONTAL
         BORDER-VERTICAL)

(define BORDER-TOP-LEFT "┌")
(define BORDER-TOP-RIGHT "┐")
(define BORDER-BOTTOM-LEFT "└")
(define BORDER-BOTTOM-RIGHT "┘")
(define BORDER-HORIZONTAL "─")
(define BORDER-VERTICAL "│")

(define (make-border-string length [char BORDER-HORIZONTAL])
  (make-string length (string-ref char 0)))

(define (draw-border! frame x y width height style)
  (define horizontal-line (make-string width (string-ref BORDER-HORIZONTAL 0)))
  (frame-set-string! frame x y horizontal-line style)
  (frame-set-string! frame x (+ y height -1) horizontal-line style)

  (let loop ([row (+ y 1)])
    (when (< row (+ y height -1))
      (frame-set-string! frame x row BORDER-VERTICAL style)
      (frame-set-string! frame (+ x width -1) row BORDER-VERTICAL style)
      (loop (+ row 1))))

  (frame-set-string! frame x y BORDER-TOP-LEFT style)
  (frame-set-string! frame (+ x width -1) y BORDER-TOP-RIGHT style)
  (frame-set-string! frame x (+ y height -1) BORDER-BOTTOM-LEFT style)
  (frame-set-string! frame (+ x width -1) (+ y height -1) BORDER-BOTTOM-RIGHT style))

(define (make-space-string length)
  (make-string length #\space))

(define (draw-text-line! frame x y text style)
  (frame-set-string! frame x y text style))

(define (draw-horizontal-line! frame
                               x
                               y
                               width
                               style
                               #:left-cap [left-cap ""]
                               #:right-cap [right-cap ""])
  (let* ([line-width (- width (string-length left-cap) (string-length right-cap))]
         [line (make-border-string line-width)]
         [full-line (string-append left-cap line right-cap)])
    (draw-text-line! frame x y full-line style)))

(define (draw-vertical-lines! frame x-positions y style)
  (for-each (lambda (x) (draw-text-line! frame x y BORDER-VERTICAL style)) x-positions))

(define (draw-box! frame
                   x
                   y
                   width
                   height
                   style
                   #:title [title #f]
                   #:content [content #f]
                   #:padding [padding 0])
  (let* ([inner-width (- width 2)]
         [has-title? (and title (> (string-length title) 0))]
         [clean-title (if has-title? title "")]
         [title-len (string-length clean-title)]
         [top-line-width (- inner-width title-len)]

         [top-line
          (if has-title?
              (string-append BORDER-TOP-LEFT
                             clean-title
                             (make-border-string top-line-width)
                             BORDER-TOP-RIGHT)
              (string-append BORDER-TOP-LEFT (make-border-string inner-width) BORDER-TOP-RIGHT))]
         [bottom-line
          (string-append BORDER-BOTTOM-LEFT (make-border-string inner-width) BORDER-BOTTOM-RIGHT)])

    (draw-text-line! frame x y top-line style)

    (let loop ([row 1])
      (when (< row (- height 1))
        (let ([current-y (+ y row)])
          (draw-text-line! frame x current-y BORDER-VERTICAL style)
          (draw-text-line! frame (+ x width -1) current-y BORDER-VERTICAL style)

          (when (and content (< (- row 1) (length content)))
            (let* ([content-line (list-ref content (- row 1))]
                   [padded-x (+ x 1 padding)]
                   [max-content-width (- inner-width (* 2 padding))]
                   [truncated-content (if (> (string-length content-line) max-content-width)
                                          (substring content-line 0 max-content-width)
                                          content-line)])
              (draw-text-line! frame padded-x current-y truncated-content style)))

          (loop (+ row 1)))))

    (draw-text-line! frame x (+ y height -1) bottom-line style)))

(struct CenteredLayout (x y width height))

(define (calculate-centered-layout container-x
                                   container-y
                                   container-width
                                   container-height
                                   content-width
                                   content-height
                                   max-width
                                   max-height
                                   center-horizontal?
                                   center-vertical?)
  (let* ([actual-width (if (and max-width (> content-width max-width)) max-width content-width)]
         [actual-height (if (and max-height (> content-height max-height)) max-height content-height)]
         [h-offset (if center-horizontal?
                       (quotient (- container-width actual-width) 2)
                       0)]
         [v-offset (if center-vertical?
                       (quotient (- container-height actual-height) 2)
                       0)]
         [final-x (+ container-x h-offset)]
         [final-y (+ container-y v-offset)])
    (CenteredLayout final-x final-y actual-width actual-height)))
