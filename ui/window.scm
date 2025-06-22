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

;; UI Constants
(define WINDOW-SIZE-RATIO 0.9) ; Window size as a ratio of screen size
(define CONTENT-PADDING 3) ; Padding between window border and content
(define BORDER-PADDING 2) ; Padding for border elements
(define TEXT-FIELD-WIDTH 40) ; Default width for text fields
(define CHECKBOX-WIDTH 5) ; Width of checkbox box
(define CHECKBOX-TEXT-GAP 1) ; Gap between checkbox and label
(define CURSOR-PADDING 2) ; Padding for cursor positioning in text fields

;; State struct
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

;; ----------------
;; Field Access Helpers
;; ----------------

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

;; ----------------
;; Text Editing Functions
;; ----------------

(define (move-cursor-left state field-id)
  (when (field-is-text? field-id)
    (let ([current-pos (get-field-cursor-pos state field-id)])
      (set-field-cursor-pos! state field-id (max 0 (- current-pos 1))))))

(define (move-cursor-right state field-id)
  (when (field-is-text? field-id)
    (let ([current-pos (get-field-cursor-pos state field-id)]
          [field-value (get-field-value state field-id)])
      (set-field-cursor-pos! state field-id (min (string-length field-value) (+ current-pos 1))))))

(define (insert-char-at-cursor state field-id char)
  (when (field-is-text? field-id)
    (let* ([field-value (get-field-value state field-id)]
           [cursor-pos (get-field-cursor-pos state field-id)]
           [before (substring field-value 0 cursor-pos)]
           [after (substring field-value cursor-pos (string-length field-value))])
      (set-field-value! state field-id (string-append before (string char) after))
      (set-field-cursor-pos! state field-id (+ cursor-pos 1)))))

(define (delete-char-at-cursor state field-id)
  (when (and (field-is-text? field-id) (> (get-field-cursor-pos state field-id) 0))
    (let* ([field-value (get-field-value state field-id)]
           [cursor-pos (get-field-cursor-pos state field-id)]
           [before (substring field-value 0 (- cursor-pos 1))]
           [after (substring field-value cursor-pos (string-length field-value))])
      (set-field-value! state field-id (string-append before after))
      (set-field-cursor-pos! state field-id (- cursor-pos 1)))))

(define (insert-text-at-cursor state field-id text)
  (when (field-is-text? field-id)
    (let* ([field-value (get-field-value state field-id)]
           [cursor-pos (get-field-cursor-pos state field-id)]
           [before (substring field-value 0 cursor-pos)]
           [after (substring field-value cursor-pos (string-length field-value))]
           [new-value (string-append before text after)])
      (set-field-value! state field-id new-value)
      (set-field-cursor-pos! state field-id (+ cursor-pos (string-length text))))))

;; ----------------
;; UI Styling
;; ----------------

;; Define a structure for holding all UI styles
(struct UIStyles
        (text ; Default text style
         popup ; Popup window background style
         bold ; Bold text style
         dim ; Dimmed text style
         search ; Search result highlighting style
         status ; Status bar style
         active ; Active field highlighting style
         ))

;; Create a central style registry for the application
(define (create-ui-styles)
  (let* ([ui-text-style (theme-scope "ui.text")]
         [ui-popup-style (theme-scope "ui.popup")])
    (UIStyles ui-text-style ; text
              ui-popup-style ; popup
              (style-with-bold ui-text-style) ; bold
              (style-with-dim ui-text-style) ; dim
              (theme-scope "search.match") ; search
              (theme-scope "ui.statusline") ; status
              (theme-scope "markup.heading")))) ; active - usually green in most themes

;; ----------------
;; Field Drawing Functions
;; ----------------

(define (draw-boolean-field frame content-x label-y field-def field-value active? styles)
  (let* ([title (field-label field-def)]
         [checkbox-content (if field-value "X" " ")]
         ;; Compute horizontal line for top/bottom of checkbox
         [horizontal-line (make-string (- CHECKBOX-WIDTH 2) (string-ref BORDER-HORIZONTAL 0))]
         ;; Create the box components
         [checkbox-top (string-append BORDER-TOP-LEFT horizontal-line BORDER-TOP-RIGHT)]
         [checkbox-middle (string-append BORDER-VERTICAL " " checkbox-content " " BORDER-VERTICAL)]
         [checkbox-bottom (string-append BORDER-BOTTOM-LEFT horizontal-line BORDER-BOTTOM-RIGHT)]
         ;; Choose style based on active state
         [box-style (if active?
                        (UIStyles-active styles)
                        (UIStyles-text styles))]
         [text-style (UIStyles-text styles)])

    ;; Draw the checkbox box
    (frame-set-string! frame content-x label-y checkbox-top box-style)
    (frame-set-string! frame content-x (+ label-y 1) checkbox-middle box-style)
    (frame-set-string! frame content-x (+ label-y 2) checkbox-bottom box-style)

    ;; Draw the title text to the right of the checkbox with appropriate spacing
    (frame-set-string! frame
                       (+ content-x CHECKBOX-WIDTH CHECKBOX-TEXT-GAP)
                       (+ label-y 1)
                       title
                       text-style)))

(define (draw-text-field-box frame
                             content-x
                             label-y
                             content-width
                             field-def
                             field-value
                             active?
                             styles)
  (let* ([box-width (min TEXT-FIELD-WIDTH (- content-width 4))]
         [title (field-label field-def)]
         [title-len (string-length title)]

         ;; Calculate box dimensions with title in the top border
         [title-start-pos 1] ; Position after BORDER-TOP-LEFT
         [right-border-pos (- box-width 1)] ; Position of the right border
         [remaining-border-width (- right-border-pos title-start-pos title-len)]

         ;; Create box elements with proper width calculations
         [box-top (string-append BORDER-TOP-LEFT
                                 title
                                 (make-string remaining-border-width (string-ref BORDER-HORIZONTAL 0))
                                 BORDER-TOP-RIGHT)]
         [box-bottom (string-append BORDER-BOTTOM-LEFT
                                    (make-string (- box-width 2) (string-ref BORDER-HORIZONTAL 0))
                                    BORDER-BOTTOM-RIGHT)]

         ;; Select styles based on active state
         [base-style (if active?
                         (UIStyles-active styles)
                         (UIStyles-text styles))]
         [title-style (if active?
                          (style-with-bold base-style)
                          base-style)]
         [text-style (UIStyles-text styles)])

    ;; Draw the box with title integrated into the top border
    (frame-set-string! frame content-x label-y box-top title-style)
    (frame-set-string! frame content-x (+ label-y 1) BORDER-VERTICAL base-style)
    (frame-set-string! frame (+ content-x (- box-width 1)) (+ label-y 1) BORDER-VERTICAL base-style)
    (frame-set-string! frame content-x (+ label-y 2) box-bottom base-style)

    ;; Draw the text value inside the box
    (frame-set-string! frame
                       (+ content-x CURSOR-PADDING)
                       (+ label-y 1)
                       (truncate-string (or field-value "") (- box-width (+ CURSOR-PADDING 2)))
                       text-style)))

;; Draws a single field based on its type
(define (draw-field frame content-x content-width field-def field-value active? field-y-pos styles)
  (let* ([field-id (field-id field-def)]
         [field-type (field-type field-def)])

    ;; Draw different field types appropriately
    (if (equal? field-type FIELD-TYPE-BOOLEAN)
        (draw-boolean-field frame content-x field-y-pos field-def field-value active? styles)
        (draw-text-field-box frame
                             content-x
                             field-y-pos
                             content-width
                             field-def
                             field-value
                             active?
                             styles))))

;; Processes and draws all fields
(define (draw-all-fields frame content-x content-y content-width current-field state styles)
  (let ([field-positions (calculate-field-positions content-y)])
    (let process-fields ([fields (get-all-fields)])
      (when (not (null? fields))
        (let* ([field-def (car fields)]
               [field-id (field-id field-def)]
               [active? (equal? current-field field-id)]
               [y-pos (car (hash-ref field-positions field-id))]
               [field-value (get-field-value state field-id)])

          ;; Draw this field
          (draw-field frame content-x content-width field-def field-value active? y-pos styles)

          ;; Process next field
          (process-fields (cdr fields)))))))

;; ----------------
;; Results Display Functions
;; ----------------

;; Draw search results with pagination (showing the most recent results that fit)
(define (draw-search-results frame
                             content-x
                             content-y
                             content-height
                             y
                             window-height
                             lines
                             result-style)
  (let* ([max-visible-lines (- content-height 1)] ; Leave room for status line
         ;; If we have more lines than can fit, take the most recent ones
         [display-lines (if (> (length lines) max-visible-lines)
                            (take-right lines max-visible-lines)
                            lines)])

    ;; Draw each result line
    (let loop ([remaining-lines display-lines]
               [current-row 0])
      (when (and (not (null? remaining-lines)) ; Still have lines to draw
                 (< current-row max-visible-lines) ; Within visible area
                 (< (+ content-y current-row) (+ y window-height -2))) ; Within window
        (let* ([line (car remaining-lines)]
               [truncated-line (truncate-string line (- content-height 4))])
          ;; Draw this line and continue with next
          (frame-set-string! frame content-x (+ content-y current-row) truncated-line result-style)
          (loop (cdr remaining-lines) (+ current-row 1)))))))

(define (draw-status-line frame
                          content-x
                          position-y ;; Y position (already calculated)
                          content-width ;; Width to use for truncation
                          lines
                          completed?
                          status-style ;; Style for completed status
                          dim-style) ;; Style for in-progress status
  (let* ([line-count (length lines)]
         [status-text
          (if completed?
              (string-append "Done. " (number->string line-count) " matches. Press any key to close")
              (string-append "Searching... " (number->string line-count) " matches"))]
         [style (if completed? status-style dim-style)])
    (frame-set-string! frame content-x position-y (truncate-string status-text content-width) style)))

;; ----------------
;; Main Rendering Function
;; ----------------

;; Layout structure for window dimensions and position
(struct WindowLayout
        (x ; Window X position
         y ; Window Y position
         width ; Total window width
         height ; Total window height
         content-x ; Content area X position
         content-y ; Content area Y position
         content-width ; Content area width
         content-height ; Content area height
         ))

;; Calculate window dimensions and position
(define (calculate-window-layout rect)
  (let* ([screen-width (area-width rect)]
         [screen-height (area-height rect)]
         ;; Calculate window size based on screen dimensions
         [window-width (exact (round (* screen-width WINDOW-SIZE-RATIO)))]
         [window-height (exact (round (* screen-height WINDOW-SIZE-RATIO)))]
         ;; Center window on screen
         [x (exact (max 1 (- (round (/ screen-width 2)) (round (/ window-width 2)))))]
         [y (exact (max 0 (- (round (/ screen-height 2)) (round (/ window-height 2)))))]
         ;; Calculate content area dimensions
         [content-x (+ x CONTENT-PADDING)]
         [content-y (+ y BORDER-PADDING)]
         [content-width (- window-width (* CONTENT-PADDING 2))]
         [content-height (- window-height (* BORDER-PADDING 2))])
    ;; Return a structured layout object
    (WindowLayout x y window-width window-height content-x content-y content-width content-height)))

;; Draw the main window frame and title
(define (draw-window-frame frame x y width height border-style title-style title)
  (let* ([window-area (area x y width height)]
         [popup-style (theme-scope "ui.popup")]
         [title-x (+ x 2)] ; Position title slightly inset from left edge
         [max-title-width (- width 4)] ; Leave room on both sides
         [truncated-title (truncate-string title max-title-width)])

    ;; Clear entire window area with popup background
    (buffer/clear-with frame window-area popup-style)

    ;; Draw the window border
    (draw-border! frame x y width height border-style)

    ;; Draw the window title
    (frame-set-string! frame title-x y truncated-title title-style)))

;; Positions the cursor for text input fields
(define (position-cursor-in-text-field state current-field field-positions content-x)
  (let ([active-field-def (get-field-by-id current-field)])
    (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
      (let* ([positions (hash-ref field-positions current-field)]
             [box-top-y (car positions)]
             [cursor-pos (get-field-cursor-pos state current-field)]
             [cursor-row (+ box-top-y 1)] ; Position is 1 row below the box top
             [cursor-col (+ content-x CURSOR-PADDING cursor-pos)]) ; Account for padding
        ;; Position cursor inside the box
        (set-position-row! (ScooterWindow-cursor-position state) cursor-row)
        (set-position-col! (ScooterWindow-cursor-position state) cursor-col)))))

;; Draw hint text at bottom of window to guide user interactions
(define (draw-hint-text frame content-x y window-height hint-style)
  (let ([hint-y (+ y window-height -2)] ; Position at bottom of window
        [hint-text
         "<tab> next field | <shift+tab> prev field | <space> toggle | <enter> search | <esc> cancel"])
    (frame-set-string! frame content-x hint-y hint-text hint-style)))

;; Main render function
(define (scooter-render state rect frame)
  (let* (;; Get the UI layout as a structured object
         [layout (calculate-window-layout rect)]

         ;; Get themed styles
         [styles (create-ui-styles)]

         ;; Get application state
         [mode (unbox (ScooterWindow-mode-box state))]
         [search-term (get-field-value state 'search)]
         [current-field (unbox (ScooterWindow-current-field-box state))]

         ;; Create appropriate window title
         [title (if (equal? mode 'input)
                    " Scooter "
                    (string-append " Results for: " search-term " "))])

    ;; Draw window frame and title
    (draw-window-frame frame
                       (WindowLayout-x layout)
                       (WindowLayout-y layout)
                       (WindowLayout-width layout)
                       (WindowLayout-height layout)
                       (UIStyles-text styles)
                       (style-with-bold (UIStyles-text styles))
                       title)

    ;; Draw mode-specific content
    (cond
      ;; Input mode - draw fields
      [(equal? mode 'input)
       (let ([field-positions (calculate-field-positions (WindowLayout-content-y layout))])
         ;; Draw all fields
         (draw-all-fields frame
                          (WindowLayout-content-x layout)
                          (WindowLayout-content-y layout)
                          (WindowLayout-content-width layout)
                          current-field
                          state
                          styles)

         ;; Position cursor in text field
         (position-cursor-in-text-field state
                                        current-field
                                        field-positions
                                        (WindowLayout-content-x layout))

         ;; Draw hint text
         (draw-hint-text frame
                         (WindowLayout-content-x layout)
                         (WindowLayout-y layout)
                         (WindowLayout-height layout)
                         (UIStyles-dim styles)))]

      ;; Results mode - show search results
      [(equal? mode 'results)
       (let ([lines (unbox (ScooterWindow-lines-box state))]
             [completed? (unbox (ScooterWindow-completed-box state))])
         ;; Draw results
         (draw-search-results frame
                              (WindowLayout-content-x layout)
                              (WindowLayout-content-y layout)
                              (WindowLayout-content-height layout)
                              (WindowLayout-y layout)
                              (WindowLayout-height layout)
                              lines
                              (UIStyles-search styles))

         ;; Draw status line
         (draw-status-line
          frame
          (WindowLayout-content-x layout)
          (+ (WindowLayout-y layout) (WindowLayout-height layout) -3) ; Position at bottom of window
          (WindowLayout-content-width layout)
          lines
          completed?
          (UIStyles-status styles)
          (UIStyles-dim styles)))])))

;; ----------------
;; Event Handling Functions
;; ----------------

;; Handles paste events in input mode
(define (handle-paste-event state paste-text)
  (when paste-text
    (let ([current-field-id (unbox (ScooterWindow-current-field-box state))])
      (when (field-is-text? current-field-id)
        (insert-text-at-cursor state current-field-id paste-text)))))

;; Handles tab key for field navigation
(define (handle-tab-key state modifier)
  (let* ([current-field-id (unbox (ScooterWindow-current-field-box state))]
         [new-field-id (if (equal? modifier key-modifier-shift)
                           (get-previous-field current-field-id)
                           (get-next-field current-field-id))])
    (set-box! (ScooterWindow-current-field-box state) new-field-id)))

;; Handles enter key to start search
(define (handle-enter-key state)
  (let ([search-term (get-field-value state 'search)]
        [replace-term (get-field-value state 'replace)])
    (when (> (string-length search-term) 0)
      (start-scooter-search state search-term replace-term))))

;; Handles character input
(define (handle-char-input state char)
  (let* ([current-field-id (unbox (ScooterWindow-current-field-box state))]
         [field-def (get-field-by-id current-field-id)])
    (when (and (char? char) field-def)
      (cond
        ;; Text field - insert char
        [(equal? (field-type field-def) FIELD-TYPE-TEXT)
         (insert-char-at-cursor state current-field-id char)]

        ;; Boolean field - toggle with space
        [(and (equal? (field-type field-def) FIELD-TYPE-BOOLEAN) (equal? char #\space))
         (let ([current-value (get-field-value state current-field-id)])
           (set-field-value! state current-field-id (not current-value)))]))))

;; Process key events in input mode
(define (handle-input-mode-event state event)
  (cond
    ;; Paste events
    [(paste-event? event) (handle-paste-event state (paste-event-string event))]

    ;; Tab key - navigate fields
    [(key-event-tab? event) (handle-tab-key state (key-event-modifier event))]

    ;; Enter key - start search
    [(key-event-enter? event) (handle-enter-key state)]

    ;; Left arrow - move cursor left
    [(key-event-left? event) (move-cursor-left state (unbox (ScooterWindow-current-field-box state)))]

    ;; Right arrow - move cursor right
    [(key-event-right? event)
     (move-cursor-right state (unbox (ScooterWindow-current-field-box state)))]

    ;; Backspace - delete character
    [(key-event-backspace? event)
     (delete-char-at-cursor state (unbox (ScooterWindow-current-field-box state)))]

    ;; Character input
    [(key-event-char event) (handle-char-input state (key-event-char event))])

  ;; Always consume events in input mode
  event-result/consume)

;; Process key events in results mode
(define (handle-results-mode-event state event)
  ;; In results mode, any key press closes the window
  (if (key-event? event) event-result/close event-result/consume))

;; Main event handler
(define (scooter-event-handler state event)
  (cond
    ;; Escape key - close window regardless of mode
    [(key-event-escape? event) event-result/close]

    ;; Route events based on current mode
    [(equal? (unbox (ScooterWindow-mode-box state)) 'input) (handle-input-mode-event state event)]

    [(equal? (unbox (ScooterWindow-mode-box state)) 'results) (handle-results-mode-event state event)]

    ;; Other modes (fallback)
    [else event-result/consume]))

;; Cursor handler
(define (scooter-cursor-handler state _)
  (and (equal? (unbox (ScooterWindow-mode-box state)) 'input)
       (field-is-text? (unbox (ScooterWindow-current-field-box state)))
       (ScooterWindow-cursor-position state)))

;; ----------------
;; Search Functions
;; ----------------

;; Start a search operation with the given search and replace terms
(define (start-scooter-search state search-term replace-term)
  ;; Initialize the search state
  (init-search-state! state search-term replace-term)

  ;; Execute the search process
  (execute-search-process! state))

;; Initialize the state for a new search
(define (init-search-state! state search-term replace-term)
  ;; Switch to results mode
  (set-box! (ScooterWindow-mode-box state) 'results)

  ;; Update the field values with the provided terms
  (set-field-value! state 'search search-term)
  (set-field-value! state 'replace replace-term)

  ;; Clear previous results
  (set-box! (ScooterWindow-lines-box state) '())
  (set-box! (ScooterWindow-completed-box state) #f))

;; Execute the search process and set up output handling
(define (execute-search-process! state)
  (let* ([field-values (unbox (ScooterWindow-field-values-box state))]
         [args (build-scooter-args field-values)]
         [cmd (command "scooter" args)])

    ;; Configure the command to pipe output
    (set-piped-stdout! cmd)

    ;; Spawn the process and get handles
    (let* ([process-result (spawn-process cmd)]
           [process (Ok->value process-result)]
           [stdout-port (child-stdout process)])

      ;; Update state with process info
      (set-box! (ScooterWindow-process-box state) process)
      (set-box! (ScooterWindow-stdout-port-box state) stdout-port)

      ;; Start asynchronous output reading
      (read-process-output-async state))))

;; Process output reading
(define (read-process-output-async state)
  (let ([port (unbox (ScooterWindow-stdout-port-box state))]
        [process (unbox (ScooterWindow-process-box state))])

    (enqueue-thread-local-callback (lambda ()
                                     (let process-line ()
                                       (let ([line (read-line-from-port port)])
                                         (cond
                                           ;; End of file - process completed
                                           [(eof-object? line)
                                            (set-box! (ScooterWindow-completed-box state) #t)
                                            (wait process)]

                                           ;; Process line and continue
                                           [else
                                            ;; Add line to results
                                            (set-box! (ScooterWindow-lines-box state)
                                                      (append (unbox (ScooterWindow-lines-box state))
                                                              (list line)))
                                            ;; Continue reading
                                            (enqueue-thread-local-callback process-line)])))))))
