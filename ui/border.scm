(require-builtin helix/components)

(provide draw-border!
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
