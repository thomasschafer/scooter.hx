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
         SearchData
         create-scooter-window
         scooter-render
         scooter-event-handler
         scooter-cursor-handler
         start-scooter-search)

(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 3)
(define BORDER-PADDING 2)
(define SCROLL-MARGIN 2)
(define STATUS-HEIGHT 1)
(define GAP-HEIGHT 1)

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
  (ScooterWindow (box 'search-fields) ; mode-box
                 (box (create-initial-field-values)) ; field-values-box
                 (box (create-initial-cursor-positions)) ; cursor-positions-box
                 (box 'search) ; current-field-box
                 (box (SearchData 0 #f '() 0)) ; lines-box - initialize with empty SearchData
                 (box #f) ; completed-box
                 (position 0 0) ; cursor-position
                 (box '()) ; debug-events-box
                 (box (Scooter-new directory
                                   #f)) ; engine-box ;; TODO: let users enable/disable logging
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
  (list (cons (SteelSearchResult-display-path result) (UIStyles-text (ui-styles)))
        (cons ":" (UIStyles-text (ui-styles)))
        (cons (int->string (SteelSearchResult-line-num result)) (UIStyles-line-num (ui-styles)))))

(define (draw-search-results frame content-x content-y content-width content-height raw-data state)

  (let* ([result-style (UIStyles-text (ui-styles))]
         [highlight-style (UIStyles-active (ui-styles))]
         [result-count (SearchData-result-count raw-data)]
         [is-complete (SearchData-is-complete raw-data)]
         [results (SearchData-results raw-data)]
         [data-scroll-offset (SearchData-scroll-offset raw-data)]
         [status-line (string-append (if is-complete "Search complete!" "Searching...")
                                     " Found "
                                     (to-string result-count)
                                     " results")]
         [selected-index (get-selected-index state)]
         [scroll-offset (get-scroll-offset state)]
         [results-start-y (+ content-y STATUS-HEIGHT GAP-HEIGHT)]
         [results-height (- content-height STATUS-HEIGHT GAP-HEIGHT)]
         [results-count (length results)])

    (set-content-height! state results-height)

    (let ([truncated-status (truncate-string status-line (- content-width 4))])
      (frame-set-string! frame content-x content-y truncated-status (UIStyles-popup (ui-styles))))

    ;; Draw search results - results are already relative to scroll offset
    (when (> results-count 0)
      (let loop ([index 0]
                 [current-row 0])
        (when (and (< index results-count) (< current-row results-height))
          (let* ([result (list-ref results index)]
                 [absolute-index (+ index data-scroll-offset)]
                 [segments (format-search-result result)]
                 [row-y (+ results-start-y current-row)]
                 [is-selected (= absolute-index selected-index)]
                 [styled-segments (if is-selected
                                      ;; Apply highlight style to all segments
                                      (map (lambda (seg) (cons (car seg) highlight-style)) segments)
                                      segments)])
            (render-styled-segments frame content-x row-y styled-segments (- content-width 4)))
          (loop (+ index 1) (+ current-row 1)))))))

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

(define RESULT-FETCH-BUFFER 10)

(struct SearchData (result-count is-complete results scroll-offset))

(define (get-search-data state)
  (let ([lines (get-lines state)]) (and (SearchData? lines) lines)))

(define (calculate-fetch-window scroll-offset content-height result-count)
  (let* ([fetch-start scroll-offset]
         [fetch-end (min (max 0 (- result-count 1))
                         (+ scroll-offset content-height RESULT-FETCH-BUFFER))])
    (values fetch-start fetch-end)))

(define (fetch-and-set-visible-results state engine result-count is-complete)
  (let* ([scroll-offset (get-scroll-offset state)]
         [content-height (get-content-height state)])
    (call-with-values
     (lambda () (calculate-fetch-window scroll-offset content-height result-count))
     (lambda (fetch-start fetch-end)
       (let ([results (if (and (>= result-count 0) (>= fetch-end fetch-start))
                          (Scooter-search-results-window engine fetch-start fetch-end)
                          '())])
         (set-box! (ScooterWindow-lines-box state)
                   (SearchData result-count is-complete results scroll-offset)))))))

(define (update-visible-results state)
  (let ([engine (get-engine state)]
        [data (get-search-data state)])
    (when data
      (let ([result-count (SearchData-result-count data)]
            [is-complete (SearchData-is-complete data)])
        (fetch-and-set-visible-results state engine result-count is-complete)))))

(define (adjust-scroll-for-selection state)
  (let ([data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)]
             [selected-index (get-selected-index state)]
             [current-scroll (get-scroll-offset state)]
             [results-height (get-content-height state)]
             [visible-start current-scroll]
             [visible-end (+ current-scroll results-height -1)]
             [new-scroll current-scroll])

        ;; Scroll down if selected is near or past the bottom
        (when (> selected-index (- visible-end SCROLL-MARGIN))
          (set! new-scroll
                (max 0
                     (min (- result-count results-height)
                          (- selected-index results-height (- SCROLL-MARGIN))))))

        ;; Scroll up if selected is near or above the top
        (when (< selected-index (+ visible-start SCROLL-MARGIN))
          (set! new-scroll (max 0 (- selected-index SCROLL-MARGIN))))

        (when (not (= new-scroll current-scroll))
          (set-scroll-offset! state new-scroll)
          ;; Re-fetch results for new scroll position
          (update-visible-results state))))))

;; Navigate by a specific amount (positive = down, negative = up)
(define (navigate-by-amount state amount)
  (let ([data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)]
             [current-selected (get-selected-index state)]
             [new-selected (max 0 (min (- result-count 1) (+ current-selected amount)))])
        (when (> result-count 0)
          (set-selected-index! state new-selected)
          (adjust-scroll-for-selection state))))))

;; Jump to specific position
(define (jump-to-position state index scroll-offset)
  (let ([data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)])
        (when (> result-count 0)
          (set-selected-index! state (max 0 (min (- result-count 1) index)))
          (set-scroll-offset! state scroll-offset)
          (update-visible-results state))))))

(define (jump-to-top state)
  (jump-to-position state 0 0))

(define (jump-to-bottom state)
  (let ([data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)]
             [results-height (get-content-height state)]
             [last-index (- result-count 1)]
             [optimal-scroll (max 0 (- result-count results-height))])
        (jump-to-position state last-index optimal-scroll)))))

(define (scroll-page state direction)
  (let ([results-height (get-content-height state)])
    (navigate-by-amount state (* direction results-height))))

(define (scroll-half-page state direction)
  (let ([results-height (get-content-height state)])
    (navigate-by-amount state (* direction (max 1 (quotient results-height 2))))))

(define (navigate-search-results state direction)
  (navigate-by-amount state direction))

(define (key-matches-char? event char)
  (and (key-event-char event) (equal? (key-event-char event) char)))

(define (key-with-ctrl? event char)
  (and (key-matches-char? event char) (equal? (key-event-modifier event) key-modifier-ctrl)))

(define (handle-search-results-mode-event state event)
  (cond
    [(or (key-event-up? event) (key-matches-char? event #\k)) (navigate-search-results state -1)]
    [(or (key-event-down? event) (key-matches-char? event #\j)) (navigate-search-results state 1)]
    [(key-with-ctrl? event #\u) (scroll-half-page state -1)]
    [(key-with-ctrl? event #\d) (scroll-half-page state 1)]
    [(key-event-page-up? event) (scroll-page state -1)]
    [(key-event-page-down? event) (scroll-page state 1)]
    [(key-matches-char? event #\g) (jump-to-top state)]
    [(key-matches-char? event #\G) (jump-to-bottom state)])
  event-result/consume)

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
  (set-mode! state 'search-results)
  (set-lines! state (SearchData 0 #f '() 0))
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
  (let ([engine (get-engine state)])
    (enqueue-thread-local-callback
     (lambda ()
       (let ([result-count (Scooter-search-result-count engine)]
             [is-complete (Scooter-search-complete? engine)])

         (fetch-and-set-visible-results state engine result-count is-complete)

         (cond
           [is-complete (set-box! (ScooterWindow-completed-box state) #t)]
           [else (enqueue-thread-local-callback (lambda () (poll-search-results state)))]))))))
