;; Main ScooterWindow component with rendering and event handling
(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/configuration.scm")
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "border.scm")
(require "input-field.scm")

(provide ScooterWindow
         scooter-render
         scooter-event-handler
         scooter-cursor-handler
         start-scooter-search
         read-process-output-async
         truncate-string
         take-right
         drop
         WINDOW-SIZE-RATIO
         CONTENT-PADDING
         BORDER-PADDING)

;; UI Constants
(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 3)
(define BORDER-PADDING 2)

;; Field access helpers (specific to ScooterWindow)
(define (get-field-value state field)
  (if (equal? field 'search)
      (ScooterWindow-search-term state)
      (ScooterWindow-replace-term state)))

(define (set-field-value! state field value)
  (if (equal? field 'search)
      (set-ScooterWindow-search-term! state value)
      (set-ScooterWindow-replace-term! state value)))

;; Cursor position helpers
(define (get-field-cursor-pos state field)
  (if (equal? field 'search)
      (ScooterWindow-search-cursor-pos state)
      (ScooterWindow-replace-cursor-pos state)))

(define (set-field-cursor-pos! state field pos)
  (if (equal? field 'search)
      (set-ScooterWindow-search-cursor-pos! state pos)
      (set-ScooterWindow-replace-cursor-pos! state pos)))

(define (move-cursor-left state field)
  (define current-pos (get-field-cursor-pos state field))
  (define new-pos (max 0 (- current-pos 1)))
  (set-field-cursor-pos! state field new-pos))

(define (move-cursor-right state field)
  (define current-pos (get-field-cursor-pos state field))
  (define field-value (get-field-value state field))
  (define new-pos (min (string-length field-value) (+ current-pos 1)))
  (set-field-cursor-pos! state field new-pos))

(define (insert-char-at-cursor state field char)
  (define field-value (get-field-value state field))
  (define cursor-pos (get-field-cursor-pos state field))
  (define before (substring field-value 0 cursor-pos))
  (define after (substring field-value cursor-pos (string-length field-value)))
  (define new-value (string-append before (string char) after))
  (set-field-value! state field new-value)
  (set-field-cursor-pos! state field (+ cursor-pos 1)))

(define (delete-char-at-cursor state field)
  (define field-value (get-field-value state field))
  (define cursor-pos (get-field-cursor-pos state field))
  (when (> cursor-pos 0)
    (define before (substring field-value 0 (- cursor-pos 1)))
    (define after (substring field-value cursor-pos (string-length field-value)))
    (define new-value (string-append before after))
    (set-field-value! state field new-value)
    (set-field-cursor-pos! state field (- cursor-pos 1))))


;; Main component state structure
(struct ScooterWindow
        (mode ; 'input or 'results
         search-term ; The search string
         replace-term ; The replace string
         current-field ; 'search or 'replace
         search-cursor-pos ; Cursor position within search text
         replace-cursor-pos ; Cursor position within replace text
         lines-box ; Search results
         process ; scooter process (or #f)
         stdout-port ; Port for reading output (or #f)
         completed-box ; Process completion status
         cursor-position ; Cursor position object for rendering
         debug-events) ; List of recent events for debugging
  #:mutable)

;; Utility functions
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

(define (take-n lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

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

  (define mode (ScooterWindow-mode state))
  (define search-term (ScooterWindow-search-term state))
  (define replace-term (ScooterWindow-replace-term state))
  (define current-field (ScooterWindow-current-field state))
  (define default-style (style))

  (define content-x (+ x CONTENT-PADDING))
  (define content-y (+ y BORDER-PADDING))
  (define content-width (- window-width (* CONTENT-PADDING 2)))
  (define content-height (- window-height (* BORDER-PADDING 2)))

  ;; Draw title
  (define title
    (if (equal? mode 'input)
        " Scooter "
        (string-append " Results for: " search-term " ")))
  (frame-set-string! frame (+ x 2) y (truncate-string title (- window-width 4)) border-style)

  (cond
    [(equal? mode 'input)
     ;; Draw both input fields
     (for-each (lambda (field)
                 (define active? (equal? current-field field))
                 (define label (field-label field active?))
                 (define value (get-field-value state field))
                 (define label-y (field-label-position field content-y))
                 (define value-y (field-position field content-y))

                 ;; Draw label and value
                 (frame-set-string! frame content-x label-y label default-style)
                 (frame-set-string! frame content-x value-y value default-style))
               '(search replace))

     ;; Set cursor position for active field
     (define active-field-y (field-position current-field content-y))
     (define active-field-cursor-pos (get-field-cursor-pos state current-field))
     (set-position-row! (ScooterWindow-cursor-position state) active-field-y)
     (set-position-col! (ScooterWindow-cursor-position state) (+ content-x active-field-cursor-pos))

     ;; Draw debug events
     (define debug-events (ScooterWindow-debug-events state))
     (when (not (null? debug-events))
       (frame-set-string! frame content-x (+ y window-height -6) "DEBUG EVENTS:" border-style)
       (let loop ([events debug-events] [row 0])
         (when (and (not (null? events)) (< row 3))
           (frame-set-string! frame content-x (+ y window-height -5 row) (car events) border-style)
           (loop (cdr events) (+ row 1)))))
     
     ;; Draw hint at bottom
     (frame-set-string! frame
                        content-x
                        (+ y window-height -2)
                        "Tab: switch fields | ←→: move cursor | Cmd+V: paste | Enter: search | Esc: cancel"
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

;; Event handler with comprehensive logging
(define (scooter-event-handler state event)
  (define mode (ScooterWindow-mode state))
  
  ;; DEBUG: Log and track every single event we receive
  (when (equal? mode 'input)
    (define event-str
      (cond
        [(key-event? event)
         (string-append "KEY: " 
                        (cond
                          [(key-event-escape? event) "ESC"]
                          [(key-event-tab? event) "TAB"]
                          [(key-event-enter? event) "ENTER"]
                          [(key-event-backspace? event) "BACKSPACE"]
                          [(key-event-left? event) "LEFT"]
                          [(key-event-right? event) "RIGHT"]
                          [(key-event-char event) (string-append "CHAR:" (string (key-event-char event)))]
                          [else "UNKNOWN-KEY"]))]
        [(mouse-event? event) "MOUSE"]
        [else (string-append "OTHER:" (to-string event))]))
    
    ;; Add to debug events list (keep last 5)
    (define current-events (ScooterWindow-debug-events state))
    (define new-events-full (cons event-str current-events))
    (define new-events (if (> (length new-events-full) 5)
                          (take-n new-events-full 5)
                          new-events-full))
    (set-ScooterWindow-debug-events! state new-events)
    
    ;; Also log to console
    (log::info! event-str))

  (cond
    ;; Always close on Escape
    [(key-event-escape? event) event-result/close]
    
    ;; Handle any non-standard events (like paste events)
    [(and (equal? mode 'input) 
          (not (key-event-escape? event))
          (not (key-event-tab? event)) 
          (not (key-event-enter? event))
          (not (key-event-backspace? event))
          (not (key-event-left? event))
          (not (key-event-right? event))
          (not (key-event-char event))
          (not (mouse-event? event)))
     ;; This might be a paste event - let's try to extract text from it
     (log::info! "HANDLING POTENTIAL PASTE EVENT")
     
     ;; Try different ways to extract text from the event
     (define event-str (to-string event))
     (log::info! (string-append "Event string: " event-str))
     
     ;; Let's just try to inspect the event structure more directly
     ;; Since we know it's Event::Paste(String), maybe we can pattern match or access fields
     (log::info! "Trying to examine event structure...")
     
     ;; Try to see if the event has any accessible properties
     (log::info! (string-append "Event type: " (if (procedure? event) "procedure" 
                                               (if (pair? event) "pair"
                                               (if (vector? event) "vector"
                                               (if (string? event) "string"
                                               "other"))))))
     
     event-result/consume]

    [(equal? mode 'input)
     (cond
       [(key-event-tab? event)
        ;; Switch between fields
        (set-ScooterWindow-current-field! state (toggle-field (ScooterWindow-current-field state)))
        event-result/consume]
       [(key-event-enter? event)
        ;; Start search
        (define search-term (ScooterWindow-search-term state))
        (define replace-term (ScooterWindow-replace-term state))
        (if (> (string-length search-term) 0)
            (begin
              (start-scooter-search state search-term replace-term)
              event-result/consume)
            event-result/consume)]
       [(key-event-left? event)
        ;; Move cursor left
        (move-cursor-left state (ScooterWindow-current-field state))
        event-result/consume]
       [(key-event-right? event)
        ;; Move cursor right
        (move-cursor-right state (ScooterWindow-current-field state))
        event-result/consume]
       [(key-event-backspace? event)
        ;; Delete character at cursor
        (delete-char-at-cursor state (ScooterWindow-current-field state))
        event-result/consume]
       [(key-event-char event)
        ;; Insert character at cursor (handles pasted text automatically)
        (define char (key-event-char event))
        (define current-field (ScooterWindow-current-field state))
        (when (char? char)
          (insert-char-at-cursor state current-field char))
        event-result/consume]
       [else event-result/consume])]

    [(equal? mode 'results)
     ;; Any key event closes in results mode
     (cond
       [(key-event? event) event-result/close]
       [else event-result/consume])]

    [else 
     ;; Catch any unhandled events - this might be our paste event!
     (when (equal? mode 'input)
       (log::info! "UNHANDLED EVENT - MIGHT BE PASTE")
       (define event-str (to-string event))
       (log::info! (string-append "Unhandled event string: " event-str))
       
       ;; Check if this is an Event (from the Event? predicate)
       (log::info! (string-append "Is Event?: " (if (Event? event) "true" "false")))
       
       ;; The Steel docs show this is the pattern: Event::Paste(String)
       ;; Maybe I need to add specific Steel binding support for paste events
       ;; For now, let's see if we can extract the string from the event representation
       
       ;; Try to parse the clipboard text from the event string representation
       ;; The format seems to be "#<helix_view::input::Event>" but this is just the debug repr
       (log::info! "Attempting to handle as paste event..."))
     event-result/consume]))

;; Cursor handler
(define (scooter-cursor-handler state _)
  ;; Only show cursor in input mode
  (if (equal? (ScooterWindow-mode state) 'input)
      (ScooterWindow-cursor-position state)
      #f))

;; Start scooter search process
(define (start-scooter-search state search-term replace-term)
  ;; Switch to results mode
  (set-ScooterWindow-mode! state 'results)

  ;; Clear previous results
  (set-box! (ScooterWindow-lines-box state) '())
  (set-box! (ScooterWindow-completed-box state) #f)

  ;; Use scooter with search and replace terms
  (define cmd (command "scooter" (list "-N" "-s" search-term "-r" replace-term)))
  (set-piped-stdout! cmd)

  ;; Spawn the process
  (define proc (~> cmd (spawn-process) (Ok->value)))
  (define stdout-port (child-stdout proc))

  ;; Update state
  (set-ScooterWindow-process! state proc)
  (set-ScooterWindow-stdout-port! state stdout-port)

  ;; Start reading output
  (read-process-output-async state))

;; Async output reading
(define (read-process-output-async state)
  (define port (ScooterWindow-stdout-port state))
  (define proc (ScooterWindow-process state))

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
