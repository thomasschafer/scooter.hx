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

;; Constants
(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 3)
(define BORDER-PADDING 2)

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
;; UI Styling Functions
;; ----------------

(define (create-ui-styles)
  (let* ([bg-color (theme->bg *helix.cx*)]
         [fg-color (theme->fg *helix.cx*)]
         [ui-text-style (theme-scope "ui.text")]
         [ui-popup-style (theme-scope "ui.popup")]
         ;; Use built-in theme colors for active elements
         [ui-cursor-style (theme-scope "ui.cursor")]
         [ui-selection-style (theme-scope "ui.selection")]
         [ui-menu-style (theme-scope "ui.menu.selected")]
         ;; Create a green-highlight style using built-in styles
         [active-style (theme-scope "markup.heading")])
    (list ui-text-style
          ui-popup-style
          (style-with-bold ui-text-style) ; active style (bold)
          (style-with-dim ui-text-style) ; hint style
          (theme-scope "search.match") ; search result style
          (theme-scope "ui.statusline") ; status style
          active-style))) ; use markup.heading which is usually green in most themes

;; ----------------
;; Field Drawing Functions
;; ----------------

(define (draw-boolean-field frame content-x label-y field-def field-value active? label-style)
  (let* ([styles (create-ui-styles)]
         [active-highlight-style (list-ref styles 6)] ; Get the active highlight style
         [box-style (if active? active-highlight-style label-style)]
         [title (field-label field-def)]
         [checkbox-content (if field-value "X" " ")]
         [checkbox-top (string-append BORDER-TOP-LEFT
                                      BORDER-HORIZONTAL
                                      BORDER-HORIZONTAL
                                      BORDER-HORIZONTAL
                                      BORDER-TOP-RIGHT)]
         [checkbox-middle (string-append BORDER-VERTICAL " " checkbox-content " " BORDER-VERTICAL)]
         [checkbox-bottom (string-append BORDER-BOTTOM-LEFT
                                         BORDER-HORIZONTAL
                                         BORDER-HORIZONTAL
                                         BORDER-HORIZONTAL
                                         BORDER-BOTTOM-RIGHT)])

    ;; Draw the checkbox box
    (frame-set-string! frame content-x label-y checkbox-top box-style)
    (frame-set-string! frame content-x (+ label-y 1) checkbox-middle box-style)
    (frame-set-string! frame content-x (+ label-y 2) checkbox-bottom box-style)

    ;; Draw the title text to the right of the checkbox
    (frame-set-string! frame (+ content-x 6) (+ label-y 1) title label-style)))

(define (draw-text-field-box frame
                             content-x
                             label-y
                             value-y
                             content-width
                             field-def
                             field-value
                             active?
                             label-style
                             value-style)
  (let* ([box-width (min 40 (- content-width 4))]
         [title (field-label field-def)]
         [title-len (string-length title)]
         [left-border-len 1]
         [right-border-len (max 1 (- box-width title-len left-border-len 2))]
         ;; Use theme-based highlight for active field
         [styles (create-ui-styles)]
         [active-highlight-style (list-ref styles 6)] ; Get the active highlight style
         [box-style (if active? active-highlight-style value-style)]
         [title-style (if active?
                          (style-with-bold box-style)
                          box-style)]
         ;; Ensure the top border has the correct width with title embedded
         [title-overlay-len (+ 1 title-len)] ;; +1 for the starting BORDER-TOP-LEFT
         [remaining-width (- box-width title-overlay-len 1)] ;; -1 for the ending BORDER-TOP-RIGHT
         [box-top (string-append BORDER-TOP-LEFT
                                 title
                                 (make-string remaining-width (string-ref BORDER-HORIZONTAL 0))
                                 BORDER-TOP-RIGHT)]
         [box-bottom (string-append BORDER-BOTTOM-LEFT
                                    (make-string (- box-width 2) (string-ref BORDER-HORIZONTAL 0))
                                    BORDER-BOTTOM-RIGHT)])

    ;; Draw the box with title integrated into the top border
    (frame-set-string! frame content-x label-y box-top title-style)
    (frame-set-string! frame content-x (+ label-y 1) BORDER-VERTICAL box-style)
    (frame-set-string! frame (+ content-x (- box-width 1)) (+ label-y 1) BORDER-VERTICAL box-style)
    (frame-set-string! frame content-x (+ label-y 2) box-bottom box-style)

    ;; Draw the text value inside the box
    (frame-set-string! frame
                       (+ content-x 2)
                       (+ label-y 1)
                       (truncate-string (or field-value "") (- box-width 4))
                       value-style)))

;; Draws a single field based on its type
(define (draw-field frame
                    content-x
                    field-def
                    field-value
                    active?
                    positions
                    default-style
                    active-style)
  (let* ([field-id (field-id field-def)]
         [label-y (car positions)]
         [value-y (cdr positions)]
         [label-style (if active? active-style default-style)]
         [value-style (if active? active-style default-style)]
         [content-width 40]) ; simplified - in real code get this from window size

    ;; Draw different field types appropriately
    (if (equal? (field-type field-def) FIELD-TYPE-BOOLEAN)
        (draw-boolean-field frame content-x label-y field-def field-value active? label-style)
        (draw-text-field-box frame
                             content-x
                             label-y
                             value-y
                             content-width
                             field-def
                             field-value
                             active?
                             label-style
                             value-style))))

;; Processes and draws all fields
(define (draw-all-fields frame content-x content-y current-field state default-style active-style)
  (let ([field-positions (calculate-field-positions content-y)])
    (let process-fields ([fields (get-all-fields)])
      (when (not (null? fields))
        (let* ([field-def (car fields)]
               [field-id (field-id field-def)]
               [active? (equal? current-field field-id)]
               [positions (hash-ref field-positions field-id)]
               [field-value (get-field-value state field-id)])

          ;; Draw this field
          (draw-field frame
                      content-x
                      field-def
                      field-value
                      active?
                      positions
                      default-style
                      active-style)

          ;; Process next field
          (process-fields (cdr fields)))))))

;; ----------------
;; Results Display Functions
;; ----------------

(define (draw-search-results frame
                             content-x
                             content-y
                             content-height
                             y
                             window-height
                             lines
                             result-style)
  (let* ([available-content-lines (- content-height 1)]
         [display-lines (if (> (length lines) available-content-lines)
                            (take-right lines available-content-lines)
                            lines)])

    ;; Draw each result line
    (let loop ([lines display-lines]
               [row 0])
      (when (and (not (null? lines))
                 (< row available-content-lines)
                 (< (+ content-y row) (+ y window-height -2)))
        (let ([line (truncate-string (car lines) (- content-height 4))])
          (frame-set-string! frame content-x (+ content-y row) line result-style)
          (loop (cdr lines) (+ row 1)))))))

(define (draw-status-line frame
                          content-x
                          position-y ;; Y position (already calculated)
                          content-width ;; Width to use for truncation
                          lines
                          completed?
                          status-style
                          dim-style)
  (let* ([line-count (length lines)]
         [status
          (if completed?
              (string-append "Done. " (number->string line-count) " matches. Press any key to close")
              (string-append "Searching... " (number->string line-count) " matches"))]
         [style (if completed? status-style dim-style)])
    (frame-set-string! frame content-x position-y (truncate-string status content-width) style)))

;; ----------------
;; Main Rendering Function
;; ----------------

;; Calculate window dimensions and position
(define (calculate-window-layout rect)
  (let* ([screen-width (area-width rect)]
         [screen-height (area-height rect)]
         [window-width (exact (round (* screen-width WINDOW-SIZE-RATIO)))]
         [window-height (exact (round (* screen-height WINDOW-SIZE-RATIO)))]
         [x (exact (max 1 (- (round (/ screen-width 2)) (round (/ window-width 2)))))]
         [y (exact (max 0 (- (round (/ screen-height 2)) (round (/ window-height 2)))))]
         [content-x (+ x CONTENT-PADDING)]
         [content-y (+ y BORDER-PADDING)]
         [content-width (- window-width (* CONTENT-PADDING 2))]
         [content-height (- window-height (* BORDER-PADDING 2))])
    (list x y window-width window-height content-x content-y content-width content-height)))

;; Draw window frame and title
(define (draw-window-frame frame x y window-width window-height border-style title-style title)
  (let ([window-area (area x y window-width window-height)]
        [popup-style (theme-scope "ui.popup")])
    ;; Clear area with popup style
    (buffer/clear-with frame window-area popup-style)
    ;; Draw the border
    (draw-border! frame x y window-width window-height border-style)
    ;; Draw the title
    (frame-set-string! frame (+ x 2) y (truncate-string title (- window-width 4)) title-style)))

;; Positions the cursor for text input fields
(define (position-cursor-in-text-field state current-field field-positions content-x)
  (let ([active-field-def (get-field-by-id current-field)])
    (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
      (let* ([positions (hash-ref field-positions current-field)]
             [box-top-y (car positions)]
             [cursor-pos (get-field-cursor-pos state current-field)])
        ;; Position cursor inside the box (1 down from box top, 2 right from box left edge)
        (set-position-row! (ScooterWindow-cursor-position state) (+ box-top-y 1))
        (set-position-col! (ScooterWindow-cursor-position state) (+ content-x 2 cursor-pos))))))

;; Draw hint at bottom of window
(define (draw-hint-text frame content-x y window-height hint-style)
  (frame-set-string!
   frame
   content-x
   (+ y window-height -2)
   "<tab> next field | <shift+tab> prev field | <space> toggle | <enter> search | <esc> cancel"
   hint-style))

;; Main render function
(define (scooter-render state rect frame)
  ;; Calculate layout
  (let* ([layout (calculate-window-layout rect)]
         [x (list-ref layout 0)]
         [y (list-ref layout 1)]
         [window-width (list-ref layout 2)]
         [window-height (list-ref layout 3)]
         [content-x (list-ref layout 4)]
         [content-y (list-ref layout 5)]
         [content-width (list-ref layout 6)]
         [content-height (list-ref layout 7)]

         ;; Get styles
         [styles (create-ui-styles)]
         [default-style (list-ref styles 0)]
         [popup-style (list-ref styles 1)]
         [active-style (list-ref styles 2)]
         [hint-style (list-ref styles 3)]
         [result-style (list-ref styles 4)]
         [status-style (list-ref styles 5)]
         [active-highlight-style (list-ref styles 6)]

         ;; Get state values
         [mode (unbox (ScooterWindow-mode-box state))]
         [search-term (get-field-value state 'search)]
         [current-field (unbox (ScooterWindow-current-field-box state))]

         ;; Create title
         [title (if (equal? mode 'input)
                    " Scooter "
                    (string-append " Results for: " search-term " "))])

    ;; Draw window frame and title
    (draw-window-frame frame
                       x
                       y
                       window-width
                       window-height
                       default-style
                       (style-with-bold default-style)
                       title)

    ;; Draw mode-specific content
    (cond
      ;; Input mode - draw fields
      [(equal? mode 'input)
       (let ([field-positions (calculate-field-positions content-y)])
         ;; Draw all fields (using active-highlight-style for active fields)
         (draw-all-fields frame
                          content-x
                          content-y
                          current-field
                          state
                          default-style
                          active-highlight-style)

         ;; Position cursor in text field
         (position-cursor-in-text-field state current-field field-positions content-x)

         ;; Draw hint text
         (draw-hint-text frame content-x y window-height hint-style))]

      ;; Results mode - show search results
      [(equal? mode 'results)
       (let ([lines (unbox (ScooterWindow-lines-box state))]
             [completed? (unbox (ScooterWindow-completed-box state))])
         ;; Draw results
         (draw-search-results frame
                              content-x
                              content-y
                              content-height
                              y
                              window-height
                              lines
                              result-style)

         ;; Draw status line
         (draw-status-line frame
                           content-x
                           (+ y window-height -3) ;; Fixed position parameter
                           content-width ;; Use content width for truncation
                           lines
                           completed?
                           status-style
                           hint-style))])))

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

;; Main event handler
(define (scooter-event-handler state event)
  (let ([mode (unbox (ScooterWindow-mode-box state))])
    (cond
      ;; Escape key - close window
      [(key-event-escape? event) event-result/close]

      ;; Input mode events
      [(equal? mode 'input)
       (cond
         ;; Paste events
         [(paste-event? event)
          (handle-paste-event state (paste-event-string event))
          event-result/consume]

         ;; Tab key - navigate fields
         [(key-event-tab? event)
          (handle-tab-key state (key-event-modifier event))
          event-result/consume]

         ;; Enter key - start search
         [(key-event-enter? event)
          (handle-enter-key state)
          event-result/consume]

         ;; Left arrow - move cursor left
         [(key-event-left? event)
          (move-cursor-left state (unbox (ScooterWindow-current-field-box state)))
          event-result/consume]

         ;; Right arrow - move cursor right
         [(key-event-right? event)
          (move-cursor-right state (unbox (ScooterWindow-current-field-box state)))
          event-result/consume]

         ;; Backspace - delete character
         [(key-event-backspace? event)
          (delete-char-at-cursor state (unbox (ScooterWindow-current-field-box state)))
          event-result/consume]

         ;; Character input
         [(key-event-char event)
          (handle-char-input state (key-event-char event))
          event-result/consume]

         ;; Other events
         [else event-result/consume])]

      ;; Results mode - any key closes
      [(equal? mode 'results) (if (key-event? event) event-result/close event-result/consume)]

      ;; Other modes
      [else event-result/consume])))

;; Cursor handler
(define (scooter-cursor-handler state _)
  (and (equal? (unbox (ScooterWindow-mode-box state)) 'input)
       (field-is-text? (unbox (ScooterWindow-current-field-box state)))
       (ScooterWindow-cursor-position state)))

;; ----------------
;; Search Functions
;; ----------------

;; Start a search operation
(define (start-scooter-search state search-term replace-term)
  ;; Switch to results mode
  (set-box! (ScooterWindow-mode-box state) 'results)

  ;; Update the field values to ensure we're using the passed terms
  (set-field-value! state 'search search-term)
  (set-field-value! state 'replace replace-term)

  ;; Clear previous results
  (set-box! (ScooterWindow-lines-box state) '())
  (set-box! (ScooterWindow-completed-box state) #f)

  ;; Prepare search command
  (let* ([field-values (unbox (ScooterWindow-field-values-box state))]
         [args (build-scooter-args field-values)]
         [cmd (command "scooter" args)])

    ;; Set up process piping
    (set-piped-stdout! cmd)

    (let* ([process-result (spawn-process cmd)]
           [process (Ok->value process-result)]
           [stdout-port (child-stdout process)])

      ;; Update state with process info
      (set-box! (ScooterWindow-process-box state) process)
      (set-box! (ScooterWindow-stdout-port-box state) stdout-port)

      ;; Start reading output
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
