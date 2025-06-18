(require-builtin steel/random as rand::)
(require-builtin helix/components)

(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/configuration.scm")
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")

(provide scooter)

;; Component state for the floating window
(struct ScooterWindow (lines-box max-lines process stdout-port))

;; Helper function for iterating with index
(define (for-each-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (when (null? lst)
          (return! void))
        (for-each-index func (cdr lst) (+ index 1)))))

;; Render function for the floating window
(define (scooter-render state rect frame)
  ;; Define the window dimensions
  (define window-width 80)
  (define window-height 30)

  ;; Center the window
  (define starting-x-offset (exact (round (/ (- (area-width rect) window-width) 2))))
  (define starting-y-offset (exact (round (/ (- (area-height rect) window-height) 2))))

  ;; Define the window area
  (define window-area (area starting-x-offset starting-y-offset window-width window-height))

  ;; Clear the area
  (buffer/clear frame window-area)

  ;; Draw border (optional - makes it look more like a window)
  (define border-style (style))

  ;; Draw top and bottom borders
  (frame-set-string! frame
                     starting-x-offset
                     starting-y-offset
                     (make-string window-width #\─)
                     border-style)
  (frame-set-string! frame
                     starting-x-offset
                     (+ starting-y-offset window-height -1)
                     (make-string window-width #\─)
                     border-style)

  ;; Draw side borders
  (let loop ([y (+ starting-y-offset 1)])
    (when (< y (+ starting-y-offset window-height -1))
      (frame-set-string! frame starting-x-offset y "│" border-style)
      (frame-set-string! frame (+ starting-x-offset window-width -1) y "│" border-style)
      (loop (+ y 1))))

  ;; Draw corners
  (frame-set-string! frame starting-x-offset starting-y-offset "┌" border-style)
  (frame-set-string! frame (+ starting-x-offset window-width -1) starting-y-offset "┐" border-style)
  (frame-set-string! frame starting-x-offset (+ starting-y-offset window-height -1) "└" border-style)
  (frame-set-string! frame
                     (+ starting-x-offset window-width -1)
                     (+ starting-y-offset window-height -1)
                     "┘"
                     border-style)

  ;; Draw title
  (define title " Running 'ls' command ")
  (frame-set-string! frame (+ starting-x-offset 2) starting-y-offset title (style))

  ;; Draw the content (lines from ls command)
  (define content-x (+ starting-x-offset 2))
  (define content-y (+ starting-y-offset 2))
  (define max-content-width (- window-width 4))
  (define max-content-height (- window-height 4))

  ;; Get the lines to display (take the last N lines if we have too many)
  (define lines (unbox (ScooterWindow-lines-box state)))
  (define display-lines
    (if (> (length lines) max-content-height)
        (take-right lines max-content-height)
        lines))

  ;; Display the lines
  (for-each-index (lambda (index line)
                    (when (< index max-content-height)
                      ;; Truncate line if too long
                      (define truncated-line
                        (if (> (string-length line) max-content-width)
                            (substring line 0 max-content-width)
                            line))
                      (frame-set-string! frame content-x (+ content-y index) truncated-line (style))))
                  display-lines
                  0)

  ;; Show status at bottom
  (define status
    (string-append " Lines: " (number->string (length lines)) " [Press any key to close] "))
  (frame-set-string! frame
                     (+ starting-x-offset 2)
                     (+ starting-y-offset window-height -2)
                     status
                     (style)))

;; Event handler for the floating window
(define (scooter-event-handler state event)
  ;; Check if process is still running and read any available lines
  (define port (ScooterWindow-stdout-port state))
  (define proc (ScooterWindow-process state))

  ;; Try to read more lines non-blockingly
  (let loop ()
    (define line (read-line-from-port port))
    (cond
      [(eof-object? line) void] ;; No more data available
      [else
       ;; Add line to our state
       (set-box! (ScooterWindow-lines-box state)
                 (append (unbox (ScooterWindow-lines-box state)) (list line)))
       (loop)]))

  ;; Close on any key press
  (if (key-event? event) event-result/close event-result/consume))

;; Helper to take last N items from a list
(define (take-right lst n)
  (define len (length lst))
  (if (<= len n)
      lst
      (drop lst (- len n))))

;; Helper to drop first N items from a list
(define (drop lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

(define (scooter)
  ;; Create and run the ls command
  (define cmd (command "ls" '("-la")))
  (set-piped-stdout! cmd)

  ;; Spawn the process
  (define proc (~> cmd (spawn-process) (Ok->value)))

  ;; Get the stdout port
  (define stdout-port (child-stdout proc))

  ;; Create initial state with empty lines
  (define state (ScooterWindow (box '()) 1000 proc stdout-port))

  ;; Define async reading function
  (define (read-output-async)
    (enqueue-thread-local-callback (lambda ()
                                     (let loop ()
                                       (define line (read-line-from-port stdout-port))
                                       (cond
                                         ;; Process completed
                                         [(eof-object? line) (wait proc)]
                                         [else
                                          ;; Add line to state
                                          (set-box! (ScooterWindow-lines-box state)
                                                    (append (unbox (ScooterWindow-lines-box state))
                                                            (list line)))
                                          ;; Continue reading
                                          (enqueue-thread-local-callback loop)])))))

  ;; Create and push the component
  (push-component!
   (new-component! "scooter-window" state scooter-render (hash "handle_event" scooter-event-handler)))

  ;; Start async reading after component is displayed
  (read-output-async))
