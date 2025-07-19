(require "helix/components.scm")
(require "helix/misc.scm")

(require "utils.scm")
(require "styles.scm")

(provide draw-box!
         draw-text-line!
         draw-horizontal-line!
         draw-vertical-lines!
         make-border-string
         make-space-string
         draw-border!
         draw-block-border!
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
         BORDER-VERTICAL
         calculate-window-area
         calculate-content-area
         calculate-split-areas
         calculate-status-area
         color-string-to-color
         create-segment-style
         render-styled-segments
         render-styled-segments-in-area)

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

(define (draw-block-border! frame area style #:border-type [border-type "plain"])
  (block/render frame area (make-block style style "all" border-type)))

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
  (let* ([line-width (- width (char-width left-cap) (char-width right-cap))]
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
         [has-title? (and title (> (char-width title) 0))]
         [clean-title (if has-title? title "")]
         [title-len (char-width clean-title)]
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
                   [truncated-content (if (> (char-width content-line) max-content-width)
                                          (char-substring content-line 0 max-content-width)
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

(define STATUS-HEIGHT 2)
(define NARROW-WINDOW-THRESHOLD 90)
(define VERTICAL-LIST-HEIGHT 5)
(define VERTICAL-PREVIEW-PADDING 1)
(define LIST-WIDTH-RATIO 2/5)

(define (color-string-to-color color-str)
  (cond
    [(equal? color-str "red") Color/Red]
    [(equal? color-str "green") Color/Green]
    [(equal? color-str "black") Color/Black]
    [else Color/Reset]))

(define (create-segment-style fg-color bg-color)
  (let ([base-style (style-fg (style) (color-string-to-color fg-color))])
    (if (equal? bg-color "")
        base-style
        (style-bg base-style (color-string-to-color bg-color)))))

(define (render-styled-segments frame x y segments max-width . fill-style)
  (let ([fill-style (if (null? fill-style)
                        #f
                        (car fill-style))])
    (let loop ([segments segments]
               [current-x x]
               [remaining-width max-width])
      (cond
        [(and (not (null? segments)) (> remaining-width 0))
         (let* ([segment (car segments)]
                [text (car segment)]
                [style (cdr segment)]
                [text-char-len (char-width text)]
                [truncated-text (if (> text-char-len remaining-width)
                                    (truncate-string text remaining-width)
                                    text)]
                [truncated-char-len (char-width truncated-text)])
           (frame-set-string! frame current-x y truncated-text style)
           (loop (cdr segments)
                 (+ current-x truncated-char-len)
                 (- remaining-width truncated-char-len)))]

        [(and (> remaining-width 0) fill-style)
         (frame-set-string! frame current-x y (make-space-string remaining-width) fill-style)]))))

(define (render-styled-segments-in-area frame area row segments . fill-style)
  (let ([args (append (list frame (area-x area) (+ (area-y area) row) segments (area-width area))
                      fill-style)])
    (apply render-styled-segments args)))

(define (calculate-window-area rect)
  (let* ([window-size-ratio 0.9]
         [screen-width (area-width rect)]
         [screen-height (area-height rect)]
         [window-width (exact (round (* screen-width window-size-ratio)))]
         [window-height (exact (round (* screen-height window-size-ratio)))]
         [x (exact (max 0 (- (round (/ screen-width 2)) (round (/ window-width 2)))))]
         [y (exact (max 0 (- (round (/ screen-height 2)) (round (/ window-height 2)))))])
    (area x y window-width window-height)))

(define (calculate-content-area window-area)
  (let ([content-padding 2]
        [help-height 1])
    (area (+ (area-x window-area) content-padding)
          (+ (area-y window-area) content-padding)
          (- (area-width window-area) (* content-padding 2))
          (- (area-height window-area) (* content-padding 2) help-height))))

(define (calculate-status-area content-area)
  (area (area-x content-area) (area-y content-area) (area-width content-area) STATUS-HEIGHT))

(define (calculate-split-areas content-area)
  (let* ([content-x (area-x content-area)]
         [content-y (area-y content-area)]
         [total-width (area-width content-area)]
         [total-height (area-height content-area)]
         [results-y (+ content-y STATUS-HEIGHT)]
         [results-height (- total-height STATUS-HEIGHT)]
         [is-narrow? (< total-width NARROW-WINDOW-THRESHOLD)])

    (if is-narrow?
        ;; Vertical layout
        (let* ([preview-y (+ results-y VERTICAL-LIST-HEIGHT 1)]
               [preview-height (- results-height VERTICAL-LIST-HEIGHT 1)])
          (values (area content-x results-y (- total-width 1) VERTICAL-LIST-HEIGHT)
                  (area (+ content-x VERTICAL-PREVIEW-PADDING)
                        preview-y
                        (- total-width (* VERTICAL-PREVIEW-PADDING 2))
                        preview-height)))

        ;; Horizontal layout
        (let* ([list-width (exact (floor (* total-width LIST-WIDTH-RATIO)))]
               [preview-x (+ content-x list-width 1)]
               [preview-width (- total-width list-width 2)])
          (values (area content-x results-y list-width results-height)
                  (area preview-x results-y preview-width results-height))))))
