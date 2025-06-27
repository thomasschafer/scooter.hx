(require "helix/components.scm")
(require "helix/misc.scm")

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-new
                          Scooter-start-search
                          Scooter-search-complete?
                          Scooter-search-result-count
                          Scooter-search-results-window
                          SteelSearchResult-display-path
                          SteelSearchResult-line-num))

(require "utils.scm")
(require "styles.scm")
(require "fields.scm")
(require "drawing.scm")

(provide ScooterWindow
         create-scooter-window
         scooter-render
         scooter-event-handler
         scooter-cursor-handler
         start-scooter-search)

(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 3)
(define BORDER-PADDING 2)

(struct ScooterWindow
        (mode-box ; 'search-fields, 'search-results
         field-values-box
         cursor-positions-box
         current-field-box
         lines-box
         completed-box
         cursor-position
         debug-events-box
         engine-box
         search-id-box ; Track current search to avoid stale callbacks
         selected-index-box ; Index of currently selected result
         scroll-offset-box ; Offset for scrolling results
         content-height-box)) ; Available height for results

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

(define (get-mode state)
  (unbox (ScooterWindow-mode-box state)))
(define (set-mode! state value)
  (set-box! (ScooterWindow-mode-box state) value))

(define (get-field-values state)
  (unbox (ScooterWindow-field-values-box state)))

(define (get-current-field state)
  (unbox (ScooterWindow-current-field-box state)))
(define (set-current-field! state value)
  (set-box! (ScooterWindow-current-field-box state) value))

(define (get-lines state)
  (unbox (ScooterWindow-lines-box state)))
(define (set-lines! state value)
  (set-box! (ScooterWindow-lines-box state) value))

(define (get-completed state)
  (unbox (ScooterWindow-completed-box state)))
(define (set-completed! state value)
  (set-box! (ScooterWindow-completed-box state) value))

(define (get-engine state)
  (unbox (ScooterWindow-engine-box state)))

(define (get-search-id state)
  (unbox (ScooterWindow-search-id-box state)))

(define (increment-search-id! state)
  (set-box! (ScooterWindow-search-id-box state) (+ 1 (unbox (ScooterWindow-search-id-box state)))))

(define (get-selected-index state)
  (unbox (ScooterWindow-selected-index-box state)))
(define (set-selected-index! state value)
  (set-box! (ScooterWindow-selected-index-box state) value))

(define (get-scroll-offset state)
  (unbox (ScooterWindow-scroll-offset-box state)))
(define (set-scroll-offset! state value)
  (set-box! (ScooterWindow-scroll-offset-box state) value))

(define (get-content-height state)
  (unbox (ScooterWindow-content-height-box state)))
(define (set-content-height! state value)
  (set-box! (ScooterWindow-content-height-box state) value))

(define (create-scooter-window directory)
  "Create a new ScooterWindow with organized initialization."
  (ScooterWindow (box 'search-fields) ; mode-box
                 (box (create-initial-field-values)) ; field-values-box
                 (box (create-initial-cursor-positions)) ; cursor-positions-box
                 (box 'search) ; current-field-box
                 (box '()) ; lines-box
                 (box #f) ; completed-box
                 (position 0 0) ; cursor-position
                 (box '()) ; debug-events-box
                 (box (Scooter-new directory)) ; engine-box
                 (box 0) ; search-id-box
                 (box 0) ; selected-index-box
                 (box 0) ; scroll-offset-box
                 (box 10))) ; content-height-box (placeholder, set during rendering)

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

(define (render-styled-segments frame x y segments max-width)
  "Render a list of (text . style) segments on a single line."
  (let loop ([segments segments]
             [current-x x]
             [remaining-width max-width])
    (when (and (not (null? segments)) (> remaining-width 0))
      (let* ([segment (car segments)]
             [text (car segment)]
             [style (cdr segment)]
             [truncated-text (if (> (string-length text) remaining-width)
                                 (truncate-string text remaining-width)
                                 text)])
        (frame-set-string! frame current-x y truncated-text style)
        (loop (cdr segments)
              (+ current-x (string-length truncated-text))
              (- remaining-width (string-length truncated-text)))))))

(define (format-search-result result)
  "Convert a raw search result to styled segments."
  (list (cons (SteelSearchResult-display-path result) (UIStyles-text (ui-styles)))
        (cons ":" (UIStyles-text (ui-styles)))
        (cons (int->string (SteelSearchResult-line-num result)) (UIStyles-line-num (ui-styles)))))

(define (draw-search-results frame
                             content-x
                             content-y
                             content-width
                             content-height
                             y
                             window-height
                             raw-data
                             state)
  (let ([result-style (UIStyles-text (ui-styles))]
        [highlight-style (UIStyles-active (ui-styles))])
    (cond
      [(and (list? raw-data) (not (null? raw-data)) (equal? (car raw-data) 'search-data))
       (let* ([result-count (cadr raw-data)]
              [is-complete (caddr raw-data)]
              [results (cadddr raw-data)]
              [status-line (string-append (if is-complete "Search complete!" "Searching...")
                                          " Found "
                                          (to-string result-count)
                                          " results")]
              [selected-index (get-selected-index state)]
              [scroll-offset (get-scroll-offset state)]
              [status-height 1]
              [gap-height 1]
              [results-start-y (+ content-y status-height gap-height)]
              [results-height (- content-height status-height gap-height 1)]
              [results-count (length results)])

         ;; Store content height for navigation
         (set-content-height! state results-height)

         ;; Draw status line at fixed position
         (let ([truncated-status (truncate-string status-line (- content-width 4))])
           (frame-set-string! frame content-x content-y truncated-status result-style))

         ;; Draw search results with scrolling
         (when (> results-count 0)
           (let loop ([index 0]
                      [current-row 0])
             (when (and (< index results-count) (< current-row results-height))
               (let ([result-index (+ index scroll-offset)])
                 (when (< result-index results-count)
                   (let* ([result (list-ref results result-index)]
                          [segments (format-search-result result)]
                          [row-y (+ results-start-y current-row)]
                          [is-selected (= result-index selected-index)]
                          [styled-segments (if is-selected
                                               ;; Apply highlight style to all segments
                                               (map (lambda (seg) (cons (car seg) highlight-style))
                                                    segments)
                                               segments)])
                     (render-styled-segments frame
                                             content-x
                                             row-y
                                             styled-segments
                                             (- content-width 4))))
                 (loop (+ index 1) (+ current-row 1)))))))]
      [else
       ;; Legacy format: handle old-style lines
       (let* ([max-visible-lines (- content-height 1)]
              [display-lines (if (> (length raw-data) max-visible-lines)
                                 (take-right raw-data max-visible-lines)
                                 raw-data)])
         (let loop ([remaining-lines display-lines]
                    [current-row 0])
           (when (and (not (null? remaining-lines))
                      (< current-row max-visible-lines)
                      (< (+ content-y current-row) (+ y window-height -2)))
             (let ([line (car remaining-lines)]
                   [row-y (+ content-y current-row)])
               (cond
                 [(and (list? line) (equal? (car line) 'styled-segments))
                  ;; Styled segments: render each segment with its own style
                  (let ([segments (cadr line)])
                    (render-styled-segments frame content-x row-y segments (- content-width 4)))]
                 [else
                  ;; Plain string: render as before
                  (let ([truncated-line (truncate-string line (- content-width 4))])
                    (frame-set-string! frame content-x row-y truncated-line result-style))])
               (loop (cdr remaining-lines) (+ current-row 1))))))])))

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

(define (draw-window-frame frame x y width height style title)
  (let* ([window-area (area x y width height)]
         [title-x (+ x 2)]
         [max-title-width (- width 4)]
         [truncated-title (truncate-string title max-title-width)])

    (buffer/clear-with frame window-area style)
    (draw-border! frame x y width height style)
    (frame-set-string! frame title-x y truncated-title style)))

(define (position-cursor-in-text-field state current-field field-positions content-x content-width)
  (let ([active-field-def (get-field-by-id current-field)])
    (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
      (let* ([positions (hash-ref field-positions current-field)]
             [box-top-y (car positions)]
             [cursor-pos (get-field-cursor-pos state current-field)]
             [cursor-row (+ box-top-y 1)]
             [cursor-col (get-field-cursor-column content-x content-width cursor-pos)])
        (set-position-row! (ScooterWindow-cursor-position state) cursor-row)
        (set-position-col! (ScooterWindow-cursor-position state) cursor-col)))))

(define (draw-hint-text frame content-x y window-height hint-style)
  (let ([hint-y (+ y window-height -2)]
        [hint-text
         "<tab> next field | <shift+tab> prev field | <space> toggle | <enter> search | <esc> cancel"])
    (frame-set-string! frame content-x hint-y hint-text hint-style)))

(define (scooter-render state rect frame)
  (let* ([layout (calculate-window-layout rect)]
         [mode (get-mode state)]
         [search-term (get-field-value state 'search)]
         [current-field (get-current-field state)]
         [title " Scooter "])

    (draw-window-frame frame
                       (WindowLayout-x layout)
                       (WindowLayout-y layout)
                       (WindowLayout-width layout)
                       (WindowLayout-height layout)
                       (UIStyles-popup (ui-styles))
                       title)

    (cond
      [(equal? mode 'search-fields)
       (let* ([all-fields (get-all-fields)]
              [field-count (length all-fields)]
              [total-fields-height (* field-count 3)] ; Each field is 3 rows high
              [hint-text-height 1] ; Space for hint text at bottom
              [content-height (- (WindowLayout-content-height layout) hint-text-height)]
              [centered-layout (calculate-centered-layout (WindowLayout-content-x layout)
                                                          (WindowLayout-content-y layout)
                                                          (WindowLayout-content-width layout)
                                                          content-height
                                                          (WindowLayout-content-width layout)
                                                          total-fields-height
                                                          #f
                                                          #f
                                                          #f
                                                          #t)]
              [field-positions (calculate-field-positions (CenteredLayout-y centered-layout))])
         (draw-all-fields frame
                          (WindowLayout-content-x layout)
                          (CenteredLayout-y centered-layout)
                          (WindowLayout-content-width layout)
                          current-field
                          state
                          get-field-value)

         (position-cursor-in-text-field state
                                        current-field
                                        field-positions
                                        (WindowLayout-content-x layout)
                                        (WindowLayout-content-width layout))

         (draw-hint-text frame
                         (WindowLayout-content-x layout)
                         (WindowLayout-y layout)
                         (WindowLayout-height layout)
                         (UIStyles-dim (ui-styles))))]

      [(equal? mode 'search-results)
       (let ([lines (get-lines state)])
         (draw-search-results frame
                              (WindowLayout-content-x layout)
                              (WindowLayout-content-y layout)
                              (WindowLayout-content-width layout)
                              (WindowLayout-content-height layout)
                              (WindowLayout-y layout)
                              (WindowLayout-height layout)
                              lines
                              state))])))

(define (handle-paste-event state paste-text)
  (when paste-text
    (let ([current-field-id (get-current-field state)])
      (when (field-is-text? current-field-id)
        (insert-text-at-cursor state current-field-id paste-text)))))

(define (handle-tab-key state modifier)
  (let* ([current-field-id (get-current-field state)]
         [new-field-id (if (equal? modifier key-modifier-shift)
                           (get-previous-field current-field-id)
                           (get-next-field current-field-id))])
    (set-current-field! state new-field-id)))

(define (handle-enter-key state)
  (let ([search-term (get-field-value state 'search)])
    (when (> (string-length search-term) 0) ; TODO: add nice error message
      (start-scooter-search state))))

(define (handle-char-input state char)
  (let* ([current-field-id (get-current-field state)]
         [field-def (get-field-by-id current-field-id)])
    (when (and (char? char) field-def)
      (cond
        [(equal? (field-type field-def) FIELD-TYPE-TEXT)
         (insert-char-at-cursor state current-field-id char)]

        [(and (equal? (field-type field-def) FIELD-TYPE-BOOLEAN) (equal? char #\space))
         (let ([current-value (get-field-value state current-field-id)])
           (set-field-value! state current-field-id (not current-value)))]))))

(define (handle-search-fields-mode-event state event)
  (cond
    [(paste-event? event) (handle-paste-event state (paste-event-string event))]

    [(key-event-tab? event) (handle-tab-key state (key-event-modifier event))]

    [(key-event-enter? event) (handle-enter-key state)]

    [(key-event-left? event) (move-cursor-left state (get-current-field state))]

    [(key-event-right? event) (move-cursor-right state (get-current-field state))]

    [(key-event-backspace? event) (delete-char-at-cursor state (get-current-field state))]

    [(key-event-char event) (handle-char-input state (key-event-char event))])

  event-result/consume)

(define (jump-to-top state)
  "Jump to the first search result."
  (let ([lines (get-lines state)])
    (when (and (list? lines) (not (null? lines)) (equal? (car lines) 'search-data))
      (let ([results (cadddr lines)])
        (when (> (length results) 0)
          (set-selected-index! state 0)
          (set-scroll-offset! state 0))))))

(define (jump-to-bottom state)
  "Jump to the last search result."
  (let ([lines (get-lines state)])
    (when (and (list? lines) (not (null? lines)) (equal? (car lines) 'search-data))
      (let* ([results (cadddr lines)]
             [results-count (length results)])
        (when (> results-count 0)
          (let* ([last-index (- results-count 1)]
                 [results-height (get-content-height state)]
                 [new-scroll (max 0 (- results-count results-height))])
            (set-selected-index! state last-index)
            (set-scroll-offset! state new-scroll)))))))

(define (scroll-page state direction)
  "Scroll by full page. Direction: -1 for up, 1 for down."
  (let ([lines (get-lines state)])
    (when (and (list? lines) (not (null? lines)) (equal? (car lines) 'search-data))
      (let* ([results (cadddr lines)]
             [results-count (length results)]
             [results-height (get-content-height state)]
             [current-selected (get-selected-index state)]
             [page-size results-height]
             [new-selected
              (max 0 (min (- results-count 1) (+ current-selected (* direction page-size))))])
        (when (> results-count 0)
          (set-selected-index! state new-selected)
          (navigate-search-results-adjust-scroll state))))))

(define (scroll-half-page state direction)
  "Scroll by half page. Direction: -1 for up, 1 for down."
  (let ([lines (get-lines state)])
    (when (and (list? lines) (not (null? lines)) (equal? (car lines) 'search-data))
      (let* ([results (cadddr lines)]
             [results-count (length results)]
             [results-height (get-content-height state)]
             [current-selected (get-selected-index state)]
             [half-page-size (max 1 (quotient results-height 2))]
             [new-selected
              (max 0 (min (- results-count 1) (+ current-selected (* direction half-page-size))))])
        (when (> results-count 0)
          (set-selected-index! state new-selected)
          (navigate-search-results-adjust-scroll state))))))

(define (navigate-search-results-adjust-scroll state)
  "Adjust scroll position based on current selection (helper for page/half-page scrolling)."
  (let ([lines (get-lines state)])
    (when (and (list? lines) (not (null? lines)) (equal? (car lines) 'search-data))
      (let* ([results (cadddr lines)]
             [results-count (length results)]
             [selected-index (get-selected-index state)]
             [current-scroll (get-scroll-offset state)]
             [results-height (get-content-height state)]
             [scroll-margin 2]
             [visible-start current-scroll]
             [visible-end (+ current-scroll results-height -1)]
             [new-scroll current-scroll])

        ;; Scroll down if selected is near or past the bottom
        (when (> selected-index (- visible-end scroll-margin))
          (set! new-scroll
                (max 0
                     (min (- results-count results-height)
                          (- selected-index results-height (- scroll-margin))))))

        ;; Scroll up if selected is near or above the top
        (when (< selected-index (+ visible-start scroll-margin))
          (set! new-scroll (max 0 (- selected-index scroll-margin))))

        (set-scroll-offset! state new-scroll)))))

(define (navigate-search-results state direction)
  "Navigate search results with smart scrolling. Direction: -1 for up, 1 for down."
  (let ([lines (get-lines state)])
    (when (and (list? lines) (not (null? lines)) (equal? (car lines) 'search-data))
      (let* ([results (cadddr lines)]
             [results-count (length results)]
             [current-selected (get-selected-index state)]
             [current-scroll (get-scroll-offset state)]
             [new-selected (max 0 (min (- results-count 1) (+ current-selected direction)))])

        (when (> results-count 0)
          (set-selected-index! state new-selected)

          ;; Smart scrolling logic
          (let* ([results-height (get-content-height state)] ; Use actual content height
                 [scroll-margin 2] ; Start scrolling when 2 from edge
                 [visible-start current-scroll]
                 [visible-end (+ current-scroll results-height -1)]
                 [new-scroll current-scroll])

            ;; Scroll down if selected is near or past the bottom
            (when (> new-selected (- visible-end scroll-margin))
              (set! new-scroll
                    (max 0
                         (min (- results-count results-height)
                              (- new-selected results-height (- scroll-margin))))))

            ;; Scroll up if selected is near or above the top
            (when (< new-selected (+ visible-start scroll-margin))
              (set! new-scroll (max 0 (- new-selected scroll-margin))))

            (set-scroll-offset! state new-scroll)))))))

(define (handle-search-results-mode-event state event)
  (cond
    ;; Up navigation: arrow up or 'k'
    [(or (key-event-up? event) (and (key-event-char event) (equal? (key-event-char event) #\k)))
     (navigate-search-results state -1)
     event-result/consume]

    ;; Down navigation: arrow down or 'j'
    [(or (key-event-down? event) (and (key-event-char event) (equal? (key-event-char event) #\j)))
     (navigate-search-results state 1)
     event-result/consume]

    ;; Half-page up: Ctrl+U
    [(and (key-event-char event)
          (equal? (key-event-char event) #\u)
          (equal? (key-event-modifier event) key-modifier-ctrl))
     (scroll-half-page state -1)
     event-result/consume]

    ;; Half-page down: Ctrl+D
    [(and (key-event-char event)
          (equal? (key-event-char event) #\d)
          (equal? (key-event-modifier event) key-modifier-ctrl))
     (scroll-half-page state 1)
     event-result/consume]

    ;; Full page up: Page Up
    [(key-event-page-up? event)
     (scroll-page state -1)
     event-result/consume]

    ;; Full page down: Page Down
    [(key-event-page-down? event)
     (scroll-page state 1)
     event-result/consume]

    ;; Jump to top: 'g'
    [(and (key-event-char event) (equal? (key-event-char event) #\g))
     (jump-to-top state)
     event-result/consume]

    ;; Jump to bottom: 'G' (Shift+g)
    [(and (key-event-char event) (equal? (key-event-char event) #\G))
     (jump-to-bottom state)
     event-result/consume]

    [(key-event? event) event-result/close]
    [else event-result/consume]))

(define (scooter-event-handler state event)
  (let ([mode (get-mode state)])
    (cond
      [(key-event-escape? event) event-result/close]
      [(equal? mode 'search-fields) (handle-search-fields-mode-event state event)]
      [(equal? mode 'search-results) (handle-search-results-mode-event state event)]
      [else event-result/consume])))

(define (scooter-cursor-handler state _)
  (and (equal? (get-mode state) 'search-fields)
       (field-is-text? (get-current-field state))
       (ScooterWindow-cursor-position state)))

(define (start-scooter-search state)
  (increment-search-id! state) ; Cancel any pending callbacks from previous searches
  (set-mode! state 'search-results)
  (set-lines! state '())
  (set-completed! state #f)
  (set-selected-index! state 0) ; Reset selection to first result
  (set-scroll-offset! state 0) ; Reset scroll to top
  (execute-search-process! state))

(define (execute-search-process! state)
  (let* ([field-values (unbox (ScooterWindow-field-values-box state))]
         [engine (get-engine state)]
         [search-term (hash-ref field-values 'search)]
         [replace-term (hash-ref field-values 'replace)]
         [fixed-strings (hash-ref field-values 'fixed-strings)]
         [match-whole-word (hash-ref field-values 'match-whole-word)]
         [match-case (hash-ref field-values 'match-case)]
         [include-pattern (hash-ref field-values 'files-include)]
         [exclude-pattern (hash-ref field-values 'files-exclude)])

    (Scooter-start-search engine
                          search-term
                          replace-term
                          fixed-strings
                          match-whole-word
                          match-case
                          include-pattern
                          exclude-pattern)

    (poll-search-results state)))

(define (get-search-status engine)
  (let ([result-count (Scooter-search-result-count engine)]
        [is-complete (Scooter-search-complete? engine)])
    (list is-complete result-count)))

(define (get-search-results engine result-count)
  (if (> result-count 0)
      (Scooter-search-results-window engine 0 (max 0 (- result-count 1)))
      '()))

(define (poll-search-results state)
  (let ([engine (get-engine state)]
        [current-search-id (get-search-id state)]) ; Capture current search ID
    (enqueue-thread-local-callback
     (lambda ()
       ;; Check if this callback is for the current search
       (when (= current-search-id (get-search-id state))
         (let ([result-count (Scooter-search-result-count engine)]
               [is-complete (Scooter-search-complete? engine)])

           (let ([results (Scooter-search-results-window
                           engine
                           0
                           (max 0 (- result-count 1)))]) ;; TODO: add scrolling support
             (set-box! (ScooterWindow-lines-box state)
                       (list 'search-data result-count is-complete results))

             (cond
               [is-complete (set-box! (ScooterWindow-completed-box state) #t)]
               [else (enqueue-thread-local-callback (lambda () (poll-search-results state)))]))))))))
