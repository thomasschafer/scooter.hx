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
(define CONTENT-PADDING 2)
(define SCROLL-MARGIN 2)
(define STATUS-HEIGHT 2)
(define GAP-HEIGHT 0)

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
                 (box (Scooter-new directory #f)) ; engine-box
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
             [remaining-width max-width]
             [last-style #f])
    (cond
      [(and (not (null? segments)) (> remaining-width 0))
       (let* ([segment (car segments)]
              [text (car segment)]
              [style (cdr segment)]
              [truncated-text (if (> (string-length text) remaining-width)
                                  (truncate-string text remaining-width)
                                  text)])
         (frame-set-string! frame current-x y truncated-text style)
         (loop (cdr segments)
               (+ current-x (string-length truncated-text))
               (- remaining-width (string-length truncated-text))
               style))]

      ;; Fill remaining width with spaces using the last style
      [(and (> remaining-width 0) last-style)
       (frame-set-string! frame current-x y (make-space-string remaining-width) last-style)])))

;; Render styled segments within a given area at a specific row
(define (render-styled-segments-in-area frame area row segments)
  (render-styled-segments frame (area-x area) (+ (area-y area) row) segments (area-width area)))

(define (format-search-result result is-selected styles)
  (let ([prefix (if is-selected " > " "   ")]
        [prefix-style (if is-selected
                          (UIStyles-selection styles)
                          (UIStyles-text styles))])
    (list (cons prefix prefix-style)
          (cons (SteelSearchResult-display-path result)
                (if is-selected
                    (UIStyles-selection styles)
                    (UIStyles-text styles)))
          (cons ":"
                (if is-selected
                    (UIStyles-selection styles)
                    (UIStyles-text styles)))
          (cons (int->string (SteelSearchResult-line-num result))
                (if is-selected
                    (UIStyles-selection styles)
                    (UIStyles-line-num styles))))))

;; Calculate sub-areas for search results rendering
(define (calculate-status-area content-area)
  (area (area-x content-area) (area-y content-area) (area-width content-area) STATUS-HEIGHT))

(define (calculate-results-area content-area)
  (area (area-x content-area)
        (+ (area-y content-area) STATUS-HEIGHT)
        (area-width content-area)
        (- (area-height content-area) STATUS-HEIGHT)))

(define (draw-search-results frame content-area raw-data state)

  (let* ([styles (ui-styles)]
         [result-style (UIStyles-text styles)]
         [highlight-style (UIStyles-selection styles)]
         [result-count (SearchData-result-count raw-data)]
         [is-complete (SearchData-is-complete raw-data)]
         [results (SearchData-results raw-data)]
         [data-scroll-offset (SearchData-scroll-offset raw-data)]
         [status-line (string-append "   "
                                     (if is-complete "Search complete!" "Searching...")
                                     " Found "
                                     (to-string result-count)
                                     " results")]
         [selected-index (get-selected-index state)]
         [scroll-offset (get-scroll-offset state)]
         [status-area (calculate-status-area content-area)]
         [results-area (calculate-results-area content-area)]
         [results-count (length results)])

    (set-content-height! state (area-height results-area))

    (buffer/clear frame content-area)
    (let ([bg-style (UIStyles-popup (ui-styles))])
      (let loop ([row 0])
        (when (< row (area-height content-area))
          (frame-set-string! frame
                             (area-x content-area)
                             (+ (area-y content-area) row)
                             (make-space-string (area-width content-area))
                             bg-style)
          (loop (+ row 1)))))

    ;; Draw status line in status area
    (let ([truncated-status (truncate-string status-line (- (area-width status-area) 4))])
      (frame-set-string! frame
                         (area-x status-area)
                         (area-y status-area)
                         truncated-status
                         (UIStyles-popup (ui-styles))))

    (when (> results-count 0)
      (let loop ([index 0]
                 [current-row 0])
        (when (and (< index results-count) (< current-row (area-height results-area)))
          (let* ([result (list-ref results index)]
                 [absolute-index (+ index data-scroll-offset)]
                 [is-selected (= absolute-index selected-index)]
                 [styled-segments (format-search-result result is-selected styles)])
            (render-styled-segments-in-area frame results-area current-row styled-segments))
          (loop (+ index 1) (+ current-row 1)))))))

(define (calculate-window-area rect)
  (let* ([screen-width (area-width rect)]
         [screen-height (area-height rect)]
         [window-width (exact (round (* screen-width WINDOW-SIZE-RATIO)))]
         [window-height (exact (round (* screen-height WINDOW-SIZE-RATIO)))]
         [x (exact (max 1 (- (round (/ screen-width 2)) (round (/ window-width 2)))))]
         [y (exact (max 0 (- (round (/ screen-height 2)) (round (/ window-height 2)))))])
    (area x y window-width window-height)))

(define (calculate-content-area window-area)
  (area (+ (area-x window-area) CONTENT-PADDING)
        (+ (area-y window-area) CONTENT-PADDING)
        (- (area-width window-area) (* CONTENT-PADDING 2))
        (- (area-height window-area) (* CONTENT-PADDING 2))))

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
  (let* ([window-area (calculate-window-area rect)]
         [content-area (calculate-content-area window-area)]
         [mode (get-mode state)]
         [search-term (get-field-value state 'search)]
         [current-field (get-current-field state)]
         [title " Scooter "]
         [popup-style (UIStyles-popup (ui-styles))])

    (buffer/clear frame window-area)
    (block/render frame window-area (make-block popup-style popup-style "all" "plain"))
    (frame-set-string! frame (+ (area-x window-area) 2) (area-y window-area) title popup-style)

    (cond
      [(equal? mode 'search-fields)
       (let* ([all-fields (get-all-fields)]
              [field-count (length all-fields)]
              [total-fields-height (* field-count 3)] ; Each field is 3 rows high
              [hint-text-height 1] ; Space for hint text at bottom
              [available-height (- (area-height content-area) hint-text-height)]
              [centered-layout (calculate-centered-layout (area-x content-area)
                                                          (area-y content-area)
                                                          (area-width content-area)
                                                          available-height
                                                          (area-width content-area)
                                                          total-fields-height
                                                          #f
                                                          #f
                                                          #f
                                                          #t)]
              [field-positions (calculate-field-positions (CenteredLayout-y centered-layout))])
         (let ([fields-area (area (area-x content-area)
                                  (CenteredLayout-y centered-layout)
                                  (area-width content-area)
                                  total-fields-height)])
           (draw-all-fields frame fields-area current-field state get-field-value))

         (position-cursor-in-text-field state
                                        current-field
                                        field-positions
                                        (area-x content-area)
                                        (area-width content-area))

         (draw-hint-text frame
                         (area-x content-area)
                         (area-y window-area)
                         (area-height window-area)
                         (UIStyles-dim (ui-styles))))]

      [(equal? mode 'search-results)
       (let ([lines (get-lines state)]) (draw-search-results frame content-area lines state))])))

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
