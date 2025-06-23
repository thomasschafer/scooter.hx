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
(require "fields.scm")

(provide ScooterWindow
         scooter-render
         scooter-event-handler
         scooter-cursor-handler
         start-scooter-search)

(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 3)
(define BORDER-PADDING 2)

(struct ScooterWindow
        (mode-box field-values-box
                  cursor-positions-box
                  current-field-box
                  lines-box
                  process-box
                  stdout-port-box
                  completed-box
                  cursor-position
                  debug-events-box))

(define-syntax define-hash-accessors
  (syntax-rules ()
    [(define-hash-accessors getter setter! state-accessor)
     (begin
       (define (getter state key)
         (hash-ref (unbox (state-accessor state)) key))

       (define (setter! state key value)
         (set-box! (state-accessor state) (hash-insert (unbox (state-accessor state)) key value))))]))

(define-hash-accessors get-field-value set-field-value! ScooterWindow-field-values-box)
(define-hash-accessors get-field-cursor-pos set-field-cursor-pos! ScooterWindow-cursor-positions-box)

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

(struct UIStyles
        (text ; Default text style
         popup ; Popup window background style
         bold ; Bold text style
         dim ; Dimmed text style
         search ; Search result highlighting style
         status ; Status bar style
         active ; Active field highlighting style
         ))

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
  (let* ([max-visible-lines (- content-height 1)]
         [display-lines (if (> (length lines) max-visible-lines)
                            (take-right lines max-visible-lines)
                            lines)])
    (let loop ([remaining-lines display-lines]
               [current-row 0])
      (when (and (not (null? remaining-lines))
                 (< current-row max-visible-lines)
                 (< (+ content-y current-row) (+ y window-height -2)))
        (let* ([line (car remaining-lines)]
               [truncated-line (truncate-string line (- content-height 4))])
          (frame-set-string! frame content-x (+ content-y current-row) truncated-line result-style)
          (loop (cdr remaining-lines) (+ current-row 1)))))))

(define (draw-status-line frame
                          content-x
                          position-y
                          content-width
                          lines
                          completed?
                          status-style
                          dim-style)
  (let* ([line-count (length lines)]
         [status-text
          (if completed?
              (string-append "Done. " (number->string line-count) " matches. Press any key to close")
              (string-append "Searching... " (number->string line-count) " matches"))]
         [style (if completed? status-style dim-style)])
    (frame-set-string! frame content-x position-y (truncate-string status-text content-width) style)))

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
    (WindowLayout x y window-width window-height content-x content-y content-width content-height)))

(define (draw-window-frame frame x y width height border-style title-style title)
  (let* ([window-area (area x y width height)]
         [popup-style (theme-scope "ui.popup")]
         [title-x (+ x 2)]
         [max-title-width (- width 4)]
         [truncated-title (truncate-string title max-title-width)])

    (buffer/clear-with frame window-area popup-style)
    (draw-border! frame x y width height border-style)
    (frame-set-string! frame title-x y truncated-title title-style)))

(define (position-cursor-in-text-field state current-field field-positions content-x)
  (let ([active-field-def (get-field-by-id current-field)])
    (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
      (let* ([positions (hash-ref field-positions current-field)]
             [box-top-y (car positions)]
             [cursor-pos (get-field-cursor-pos state current-field)]
             [cursor-row (+ box-top-y 1)]
             [cursor-col (get-field-cursor-column content-x cursor-pos)])
        (set-position-row! (ScooterWindow-cursor-position state) cursor-row)
        (set-position-col! (ScooterWindow-cursor-position state) cursor-col)))))

(define (draw-hint-text frame content-x y window-height hint-style)
  (let ([hint-y (+ y window-height -2)]
        [hint-text
         "<tab> next field | <shift+tab> prev field | <space> toggle | <enter> search | <esc> cancel"])
    (frame-set-string! frame content-x hint-y hint-text hint-style)))

(define (scooter-render state rect frame)
  (let* ([layout (calculate-window-layout rect)]
         [styles (create-ui-styles)]
         [mode (unbox (ScooterWindow-mode-box state))]
         [search-term (get-field-value state 'search)]
         [current-field (unbox (ScooterWindow-current-field-box state))]
         [title (if (equal? mode 'input)
                    " Scooter "
                    (string-append " Results for: " search-term " "))])

    (draw-window-frame frame
                       (WindowLayout-x layout)
                       (WindowLayout-y layout)
                       (WindowLayout-width layout)
                       (WindowLayout-height layout)
                       (UIStyles-text styles)
                       (style-with-bold (UIStyles-text styles))
                       title)

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
                          (UIStyles-text styles)
                          (UIStyles-active styles)
                          get-field-value)

         (position-cursor-in-text-field state
                                        current-field
                                        field-positions
                                        (WindowLayout-content-x layout))

         (draw-hint-text frame
                         (WindowLayout-content-x layout)
                         (WindowLayout-y layout)
                         (WindowLayout-height layout)
                         (UIStyles-dim styles)))]

      [(equal? mode 'results)
       (let ([lines (unbox (ScooterWindow-lines-box state))]
             [completed? (unbox (ScooterWindow-completed-box state))])
         (draw-search-results frame
                              (WindowLayout-content-x layout)
                              (WindowLayout-content-y layout)
                              (WindowLayout-content-height layout)
                              (WindowLayout-y layout)
                              (WindowLayout-height layout)
                              lines
                              (UIStyles-search styles))

         (draw-status-line frame
                           (WindowLayout-content-x layout)
                           (+ (WindowLayout-y layout) (WindowLayout-height layout) -3)
                           (WindowLayout-content-width layout)
                           lines
                           completed?
                           (UIStyles-status styles)
                           (UIStyles-dim styles)))])))

(define (handle-paste-event state paste-text)
  (when paste-text
    (let ([current-field-id (unbox (ScooterWindow-current-field-box state))])
      (when (field-is-text? current-field-id)
        (insert-text-at-cursor state current-field-id paste-text)))))

(define (handle-tab-key state modifier)
  (let* ([current-field-id (unbox (ScooterWindow-current-field-box state))]
         [new-field-id (if (equal? modifier key-modifier-shift)
                           (get-previous-field current-field-id)
                           (get-next-field current-field-id))])
    (set-box! (ScooterWindow-current-field-box state) new-field-id)))

(define (handle-enter-key state)
  (let ([search-term (get-field-value state 'search)]
        [replace-term (get-field-value state 'replace)])
    (when (> (string-length search-term) 0)
      (start-scooter-search state search-term replace-term))))

(define (handle-char-input state char)
  (let* ([current-field-id (unbox (ScooterWindow-current-field-box state))]
         [field-def (get-field-by-id current-field-id)])
    (when (and (char? char) field-def)
      (cond
        [(equal? (field-type field-def) FIELD-TYPE-TEXT)
         (insert-char-at-cursor state current-field-id char)]

        [(and (equal? (field-type field-def) FIELD-TYPE-BOOLEAN) (equal? char #\space))
         (let ([current-value (get-field-value state current-field-id)])
           (set-field-value! state current-field-id (not current-value)))]))))

(define (handle-input-mode-event state event)
  (cond
    [(paste-event? event) (handle-paste-event state (paste-event-string event))]

    [(key-event-tab? event) (handle-tab-key state (key-event-modifier event))]

    [(key-event-enter? event) (handle-enter-key state)]

    [(key-event-left? event) (move-cursor-left state (unbox (ScooterWindow-current-field-box state)))]

    [(key-event-right? event)
     (move-cursor-right state (unbox (ScooterWindow-current-field-box state)))]

    [(key-event-backspace? event)
     (delete-char-at-cursor state (unbox (ScooterWindow-current-field-box state)))]

    [(key-event-char event) (handle-char-input state (key-event-char event))])

  event-result/consume)

(define (handle-results-mode-event state event)
  (if (key-event? event) event-result/close event-result/consume))

(define (scooter-event-handler state event)
  (cond
    [(key-event-escape? event) event-result/close]

    [(equal? (unbox (ScooterWindow-mode-box state)) 'input) (handle-input-mode-event state event)]

    [(equal? (unbox (ScooterWindow-mode-box state)) 'results) (handle-results-mode-event state event)]

    [else event-result/consume]))

(define (scooter-cursor-handler state _)
  (and (equal? (unbox (ScooterWindow-mode-box state)) 'input)
       (field-is-text? (unbox (ScooterWindow-current-field-box state)))
       (ScooterWindow-cursor-position state)))

(define (start-scooter-search state search-term replace-term)
  (init-search-state! state search-term replace-term)
  (execute-search-process! state))

(define (init-search-state! state search-term replace-term)
  (set-box! (ScooterWindow-mode-box state) 'results)

  (set-field-value! state 'search search-term)
  (set-field-value! state 'replace replace-term)

  (set-box! (ScooterWindow-lines-box state) '())
  (set-box! (ScooterWindow-completed-box state) #f))

(define (execute-search-process! state)
  (let* ([field-values (unbox (ScooterWindow-field-values-box state))]
         [args (build-scooter-args field-values)]
         [cmd (command "scooter" args)])

    (set-piped-stdout! cmd)

    (let* ([process-result (spawn-process cmd)]
           [process (Ok->value process-result)]
           [stdout-port (child-stdout process)])

      (set-box! (ScooterWindow-process-box state) process)
      (set-box! (ScooterWindow-stdout-port-box state) stdout-port)

      (read-process-output-async state))))

(define (read-process-output-async state)
  (let ([port (unbox (ScooterWindow-stdout-port-box state))]
        [process (unbox (ScooterWindow-process-box state))])

    (enqueue-thread-local-callback (lambda ()
                                     (let process-line ()
                                       (let ([line (read-line-from-port port)])
                                         (cond
                                           [(eof-object? line)
                                            (set-box! (ScooterWindow-completed-box state) #t)
                                            (wait process)]

                                           [else
                                            (set-box! (ScooterWindow-lines-box state)
                                                      (append (unbox (ScooterWindow-lines-box state))
                                                              (list line)))
                                            (enqueue-thread-local-callback process-line)])))))))
