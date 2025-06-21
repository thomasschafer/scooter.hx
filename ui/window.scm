(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "border.scm")
(require "input-field.scm")
(require "field-registry.scm")
(require "field-utils.scm")
(require "command-builder.scm")
(require "utils.scm")

(provide ScooterWindow
         scooter-render
         scooter-event-handler
         scooter-cursor-handler
         start-scooter-search)

(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 3)
(define BORDER-PADDING 2)

;; Field access helpers
(define (get-field-value state field-id)
  (hash-ref (unbox (ScooterWindow-field-values-box state)) field-id))

(define (set-field-value! state field-id value)
  (set-box! (ScooterWindow-field-values-box state)
            (hash-insert (unbox (ScooterWindow-field-values-box state)) field-id value)))

(define (get-field-cursor-pos state field-id)
  (hash-ref (unbox (ScooterWindow-cursor-positions-box state)) field-id))

(define (set-field-cursor-pos! state field-id pos)
  (set-box! (ScooterWindow-cursor-positions-box state)
            (hash-insert (unbox (ScooterWindow-cursor-positions-box state)) field-id pos)))

;; Text editing functions
(define (move-cursor-left state field-id)
  (when (field-is-text? field-id)
    (define current-pos (get-field-cursor-pos state field-id))
    (set-field-cursor-pos! state field-id (max 0 (- current-pos 1)))))

(define (move-cursor-right state field-id)
  (when (field-is-text? field-id)
    (define current-pos (get-field-cursor-pos state field-id))
    (define field-value (get-field-value state field-id))
    (set-field-cursor-pos! state field-id (min (string-length field-value) (+ current-pos 1)))))

(define (insert-char-at-cursor state field-id char)
  (when (field-is-text? field-id)
    (define field-value (get-field-value state field-id))
    (define cursor-pos (get-field-cursor-pos state field-id))
    (define before (substring field-value 0 cursor-pos))
    (define after (substring field-value cursor-pos (string-length field-value)))
    (set-field-value! state field-id (string-append before (string char) after))
    (set-field-cursor-pos! state field-id (+ cursor-pos 1))))

(define (delete-char-at-cursor state field-id)
  (when (and (field-is-text? field-id) (> (get-field-cursor-pos state field-id) 0))
    (define field-value (get-field-value state field-id))
    (define cursor-pos (get-field-cursor-pos state field-id))
    (define before (substring field-value 0 (- cursor-pos 1)))
    (define after (substring field-value cursor-pos (string-length field-value)))
    (set-field-value! state field-id (string-append before after))
    (set-field-cursor-pos! state field-id (- cursor-pos 1))))

(struct ScooterWindow
        (mode-box ; 'input or 'results (boxed)
         field-values-box ; Hash table of field-id -> value (boxed)
         cursor-positions-box ; Hash table of field-id -> cursor position (boxed)
         current-field-box ; Current field ID (symbol, boxed)
         lines-box ; Search results (boxed)
         process-box ; scooter process (boxed, or #f)
         stdout-port-box ; Port for reading output (boxed, or #f)
         completed-box ; Process completion status (boxed)
         cursor-position ; Cursor position object for rendering (immutable)
         debug-events-box)) ; List of recent events for debugging (boxed)

;; Main render function
(define (scooter-render state rect frame)
  (define screen-width (area-width rect))
  (define screen-height (area-height rect))

  (define window-width (exact (round (* screen-width WINDOW-SIZE-RATIO))))
  (define window-height (exact (round (* screen-height WINDOW-SIZE-RATIO))))

  (define x (exact (max 1 (- (round (/ screen-width 2)) (round (/ window-width 2))))))
  (define y (exact (max 0 (- (round (/ screen-height 2)) (round (/ window-height 2))))))

  (define window-area (area x y window-width window-height))
  (buffer/clear frame window-area)

  (define border-style (style))
  (draw-border! frame x y window-width window-height border-style)

  (define mode (unbox (ScooterWindow-mode-box state)))
  (define search-term (get-field-value state 'search))
  (define replace-term (get-field-value state 'replace))
  (define current-field (unbox (ScooterWindow-current-field-box state)))
  (define default-style (style))

  (define content-x (+ x CONTENT-PADDING))
  (define content-y (+ y BORDER-PADDING))
  (define content-width (- window-width (* CONTENT-PADDING 2)))
  (define content-height (- window-height (* BORDER-PADDING 2)))

  (define title
    (if (equal? mode 'input)
        " Scooter "
        (string-append " Results for: " search-term " ")))
  (frame-set-string! frame (+ x 2) y (truncate-string title (- window-width 4)) border-style)

  (cond
    [(equal? mode 'input)
     (define field-positions (calculate-field-positions content-y))
     (for-each (lambda (field-def)
                 (define field-id (field-id field-def))
                 (define active? (equal? current-field field-id))
                 (define positions (hash-ref field-positions field-id))
                 (define label-y (car positions))
                 (define value-y (cdr positions))

                 ;; Draw label
                 (define prefix (if active? "> " "  "))
                 (define label-text (string-append prefix (field-label field-def)))
                 (frame-set-string! frame content-x label-y label-text default-style)

                 ;; Draw value based on field type
                 (define field-value (get-field-value state field-id))
                 (define display-value
                   (cond
                     [(equal? (field-type field-def) FIELD-TYPE-BOOLEAN) (if field-value "[X]" "[ ]")]
                     [else field-value]))
                 (frame-set-string! frame content-x value-y display-value default-style))
               (get-all-fields))

     ;; Set cursor position for active field (only for text fields)
     (define active-field-def (get-field-by-id current-field))
     (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
       (define positions (hash-ref field-positions current-field))
       (define value-y (cdr positions))
       (define cursor-pos (get-field-cursor-pos state current-field))
       (set-position-row! (ScooterWindow-cursor-position state) value-y)
       (set-position-col! (ScooterWindow-cursor-position state) (+ content-x cursor-pos)))

     ;; Draw hint at bottom
     (frame-set-string!
      frame
      content-x
      (+ y window-height -2)
      "Tab/Shift+Tab: navigate | Space: toggle | Paste: Ctrl+V | Enter: search | Esc: cancel"
      border-style)]

    [(equal? mode 'results)
     ;; Draw search results
     (define lines (unbox (ScooterWindow-lines-box state)))
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

     ;; Status line
     (define completed? (unbox (ScooterWindow-completed-box state)))
     (define line-count (length lines))
     (define status
       (if completed?
           (string-append "Done. " (number->string line-count) " matches. Press any key to close")
           (string-append "Searching... " (number->string line-count) " matches")))
     (frame-set-string! frame
                        content-x
                        (+ y window-height -3)
                        (truncate-string status content-width)
                        border-style)]))

;; Event handler
(define (scooter-event-handler state event)
  (define mode (unbox (ScooterWindow-mode-box state)))

  (cond
    [(key-event-escape? event) event-result/close]

    ;; Handle paste events
    [(and (equal? mode 'input) (paste-event? event))
     (define paste-text (paste-event-string event))
     (when paste-text
       (define current-field-id (unbox (ScooterWindow-current-field-box state)))
       (when (field-is-text? current-field-id)
         ;; Insert the pasted text at cursor position
         (define field-value (get-field-value state current-field-id))
         (define cursor-pos (get-field-cursor-pos state current-field-id))
         (define before (substring field-value 0 cursor-pos))
         (define after (substring field-value cursor-pos (string-length field-value)))
         (define new-value (string-append before paste-text after))
         (set-field-value! state current-field-id new-value)
         (set-field-cursor-pos! state current-field-id (+ cursor-pos (string-length paste-text)))))
     event-result/consume]

    [(equal? mode 'input)
     (cond
       [(key-event-tab? event)
        (define current-field-id (unbox (ScooterWindow-current-field-box state)))
        (define new-field-id
          (if (equal? (key-event-modifier event) key-modifier-shift)
              (get-previous-field current-field-id)
              (get-next-field current-field-id)))
        (set-box! (ScooterWindow-current-field-box state) new-field-id)
        event-result/consume]
       [(key-event-enter? event)
        (define search-term (get-field-value state 'search))
        (define replace-term (get-field-value state 'replace))
        (if (> (string-length search-term) 0)
            (begin
              (start-scooter-search state search-term replace-term)
              event-result/consume)
            event-result/consume)]
       [(key-event-left? event)
        (move-cursor-left state (unbox (ScooterWindow-current-field-box state)))
        event-result/consume]
       [(key-event-right? event)
        (move-cursor-right state (unbox (ScooterWindow-current-field-box state)))
        event-result/consume]
       [(key-event-backspace? event)
        (delete-char-at-cursor state (unbox (ScooterWindow-current-field-box state)))
        event-result/consume]
       [(key-event-char event)
        (define char (key-event-char event))
        (define current-field-id (unbox (ScooterWindow-current-field-box state)))
        (define field-def (get-field-by-id current-field-id))
        (when (and (char? char) field-def)
          (cond
            [(equal? (field-type field-def) FIELD-TYPE-TEXT)
             (insert-char-at-cursor state current-field-id char)]
            [(and (equal? (field-type field-def) FIELD-TYPE-BOOLEAN) (equal? char #\space))
             (define current-value (get-field-value state current-field-id))
             (set-field-value! state current-field-id (not current-value))]))
        event-result/consume]
       [else event-result/consume])]

    [(equal? mode 'results)
     (cond
       [(key-event? event) event-result/close]
       [else event-result/consume])]

    [else event-result/consume]))

(define (scooter-cursor-handler state _)
  (and (equal? (unbox (ScooterWindow-mode-box state)) 'input)
       (field-is-text? (unbox (ScooterWindow-current-field-box state)))
       (ScooterWindow-cursor-position state)))

(define (start-scooter-search state search-term replace-term)
  ;; Switch to results mode
  (set-box! (ScooterWindow-mode-box state) 'results)

  ;; Clear previous results
  (set-box! (ScooterWindow-lines-box state) '())
  (set-box! (ScooterWindow-completed-box state) #f)

  (define field-values (unbox (ScooterWindow-field-values-box state)))
  (define args (build-scooter-args field-values))

  (define cmd (command "scooter" args))
  (set-piped-stdout! cmd)

  ;; Spawn the process
  (define proc (~> cmd (spawn-process) (Ok->value)))
  (define stdout-port (child-stdout proc))

  ;; Update state
  (set-box! (ScooterWindow-process-box state) proc)
  (set-box! (ScooterWindow-stdout-port-box state) stdout-port)

  ;; Start reading output
  (read-process-output-async state))

;; Async output reading
(define (read-process-output-async state)
  (define port (unbox (ScooterWindow-stdout-port-box state)))
  (define proc (unbox (ScooterWindow-process-box state)))

  (enqueue-thread-local-callback (lambda ()
                                   (let loop ()
                                     (define line (read-line-from-port port))
                                     (cond
                                       [(eof-object? line)
                                        ;; Process completed
                                        (set-box! (ScooterWindow-completed-box state) #t)
                                        (wait proc)]
                                       [else
                                        ;; Add line to results
                                        (set-box! (ScooterWindow-lines-box state)
                                                  (append (unbox (ScooterWindow-lines-box state))
                                                          (list line)))
                                        ;; Continue reading
                                        (enqueue-thread-local-callback loop)])))))
