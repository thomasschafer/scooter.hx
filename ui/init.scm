(require-builtin helix/components)

(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/configuration.scm")
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")

(provide scooter)

(define BORDER-TOP-LEFT "┌")
(define BORDER-TOP-RIGHT "┐")
(define BORDER-BOTTOM-LEFT "└")
(define BORDER-BOTTOM-RIGHT "┘")
(define BORDER-HORIZONTAL "─")
(define BORDER-VERTICAL "│")

(struct ScooterWindow (lines-box command process stdout-port completed-box))

(define (truncate-string str max-width)
  (if (> (string-length str) max-width)
      (substring str 0 max-width)
      str))

(define (take-right lst n)
  (define len (length lst))
  (if (<= len n)
      lst
      (drop lst (- len n))))

(define (drop lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

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

(define (scooter-render state rect frame)
  (define screen-width (area-width rect))
  (define screen-height (area-height rect))

  (define window-width (exact (round (* screen-width 0.9))))
  (define window-height (exact (round (* screen-height 0.9))))

  (define x (exact (max 1 (- (round (/ screen-width 2)) (round (/ window-width 2))))))
  (define y (exact (max 0 (- (round (/ screen-height 2)) (round (/ window-height 2))))))

  (define window-area (area x y window-width window-height))
  (buffer/clear frame window-area)

  (define border-style (style))
  (draw-border! frame x y window-width window-height border-style)

  (define lines (unbox (ScooterWindow-lines-box state)))
  (define command (ScooterWindow-command state))
  (define default-style (style))

  (define content-x (+ x 3))
  (define content-y (+ y 2))
  (define content-width (- window-width 6))
  (define content-height (- window-height 4))

  (define title (truncate-string (string-append " " command " ") (- window-width 4)))
  (frame-set-string! frame (+ x 2) y title border-style)

  (define available-content-lines (- content-height 1))
  (define display-lines
    (if (> (length lines) available-content-lines)
        (take-right lines available-content-lines)
        lines))

  (let loop ([lines display-lines]
             [row 0])
    (when (and (not (null? lines))
               (< row available-content-lines)
               (< (+ content-y row) (+ y window-height -2)))
      (define line (truncate-string (car lines) content-width))
      (frame-set-string! frame content-x (+ content-y row) line default-style)
      (loop (cdr lines) (+ row 1))))

  (define completed? (unbox (ScooterWindow-completed-box state)))
  (define line-count (length lines))
  (define status
    (if completed?
        (string-append "Done. " (number->string line-count) " lines")
        (string-append "Running... " (number->string line-count) " lines")))
  (frame-set-string! frame
                     content-x
                     (+ y window-height -3)
                     (truncate-string status content-width)
                     border-style))

(define (scooter-event-handler state event)
  (if (key-event? event) event-result/close event-result/consume))

(define (read-process-output-async state)
  (define port (ScooterWindow-stdout-port state))
  (define proc (ScooterWindow-process state))

  (enqueue-thread-local-callback (lambda ()
                                   (let loop ()
                                     (define line (read-line-from-port port))
                                     (cond
                                       [(eof-object? line)
                                        (set-box! (ScooterWindow-completed-box state) #t)
                                        (wait proc)]
                                       [else
                                        (set-box! (ScooterWindow-lines-box state)
                                                  (append (unbox (ScooterWindow-lines-box state))
                                                          (list line)))
                                        (enqueue-thread-local-callback loop)])))))

(define (scooter)
  (define cmd-string "ls -la")
  (define cmd (command "ls" '("-la")))
  (set-piped-stdout! cmd)

  (define proc (~> cmd (spawn-process) (Ok->value)))
  (define stdout-port (child-stdout proc))

  (define state (ScooterWindow (box '()) cmd-string proc stdout-port (box #f)))

  (push-component!
   (new-component! "scooter-window" state scooter-render (hash "handle_event" scooter-event-handler)))

  (read-process-output-async state))
