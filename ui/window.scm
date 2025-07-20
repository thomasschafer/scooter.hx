(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require (prefix-in helix. "helix/commands.scm"))

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-new
                          Scooter-reset
                          Scooter-start-search
                          Scooter-cancel-search
                          Scooter-search-complete?
                          Scooter-search-result-count
                          Scooter-search-results-window
                          Scooter-toggle-inclusion
                          Scooter-toggle-all
                          Scooter-start-replace
                          Scooter-num-replacements-complete
                          Scooter-replacement-complete?
                          Scooter-replacement-stats
                          Scooter-cancel-replacement
                          Scooter-replacement-errors
                          ReplacementStats-num-successes
                          ReplacementStats-num-ignored
                          ReplacementStats-num-errors
                          SteelSearchResult-display-path
                          SteelSearchResult-full-path
                          SteelSearchResult-line-num
                          SteelSearchResult-line
                          SteelSearchResult-replacement
                          SteelSearchResult-included
                          SteelSearchResult-display-error
                          SteelSearchResult-build-preview
                          unicode-display-width
                          unicode-truncate-to-width
                          TextField?
                          TextField-new
                          TextField-text
                          TextField-cursor-pos
                          TextField-move-cursor-left
                          TextField-move-cursor-start
                          TextField-move-cursor-right
                          TextField-move-cursor-end
                          TextField-enter-char
                          TextField-delete-char
                          TextField-delete-char-forward
                          TextField-move-cursor-back-word
                          TextField-delete-word-backward
                          TextField-move-cursor-forward-word
                          TextField-delete-word-forward
                          TextField-delete-to-start
                          TextField-insert-text))

(require "drawing.scm")
(require "fields.scm")
(require "utils.scm")
(require "styles.scm")

(provide ScooterWindow
         create-scooter-window
         scooter-render
         scooter-event-handler
         scooter-cursor-handler
         cancel-all-operations!)

(struct SearchFieldsState
        (field-values-box cursor-positions-box current-field-box field-errors-box general-error-box))

(struct SearchResultsState
        (result-count-box is-complete-box
                          results-box
                          selected-index-box
                          scroll-offset-box
                          content-height-box
                          preserved-fields-state))

(struct SearchPerformingReplacementState ())

(struct SearchReplacementCompleteState (error-scroll-offset-box))

(struct ScooterWindow
        (current-screen-box ; holds one of the above state structs
         cursor-position
         engine-box))

(define-syntax define-hash-accessors
  (syntax-rules ()
    [(define-hash-accessors getter setter! state-accessor)
     (begin
       (define (getter state key)
         (hash-ref (unbox (state-accessor state)) key))

       (define (setter! state key value)
         (set-box! (state-accessor state) (hash-insert (unbox (state-accessor state)) key value))))]))

(define (get-current-screen state)
  (unbox (ScooterWindow-current-screen-box state)))

(define (set-current-screen! state screen-state)
  (set-box! (ScooterWindow-current-screen-box state) screen-state))

(define (get-engine state)
  (unbox (ScooterWindow-engine-box state)))

;; Internal hash accessors for SearchFieldsState
(define-hash-accessors get-field-value-internal
                       set-field-value-internal!
                       SearchFieldsState-field-values-box)
(define-hash-accessors get-field-cursor-pos-internal
                       set-field-cursor-pos-internal!
                       SearchFieldsState-cursor-positions-box)
(define-hash-accessors get-field-errors-internal
                       set-field-errors-internal!
                       SearchFieldsState-field-errors-box)

(define (get-field-text state field-id)
  (let ([screen-state (get-current-screen state)])
    (if (SearchFieldsState? screen-state)
        (let ([field-value (get-field-value-internal screen-state field-id)])
          (if (TextField? field-value)
              (TextField-text field-value)
              field-value))
        (error "get-field-text called when not SearchFieldsState"))))

(define (clear-all-errors! state)
  (clear-all-field-errors! state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (set-box! (SearchFieldsState-general-error-box screen-state) #f))))

(define (create-default-search-fields-state)
  (SearchFieldsState (box (create-initial-field-values))
                     (box (create-initial-cursor-positions))
                     (box 'search)
                     (box (hash))
                     (box #f)))

(define (create-default-search-results-state preserved-fields)
  (SearchResultsState (box 0) ; result-count-box
                      (box #f) ; is-complete-box
                      (box '()) ; results-box
                      (box 0) ; selected-index-box
                      (box 0) ; scroll-offset-box
                      (box 10) ; content-height-box
                      preserved-fields)) ; preserved-fields-state

(define (create-default-replacement-complete-state)
  (SearchReplacementCompleteState (box 0)))

(define (create-scooter-window)
  (let ([directory (get-helix-cwd)])
    (ScooterWindow (box (create-default-search-fields-state))
                   (position 0 0)
                   (box (Scooter-new directory #f)))))

(define (reset-scooter-state! state)
  (cancel-all-operations! state)
  (let ([engine (get-engine state)])
    (Scooter-reset engine)
    (set-current-screen! state (create-default-search-fields-state))))

(define (cancel-all-operations! state)
  (let ([engine (get-engine state)])
    (Scooter-cancel-search engine)
    (Scooter-cancel-replacement engine)))

(define (clear-all-field-errors! state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (set-box! (SearchFieldsState-field-errors-box screen-state) (hash)))))

(define (clear-field-error! state field-id)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (let ([errors (unbox (SearchFieldsState-field-errors-box screen-state))])
        (when (hash-contains? errors field-id)
          (set-box! (SearchFieldsState-field-errors-box screen-state)
                    (hash-remove errors field-id)))))))

(define (get-field-errors-safe state field-id)
  (let ([screen-state (get-current-screen state)])
    (if (SearchFieldsState? screen-state)
        (or (hash-try-get (unbox (SearchFieldsState-field-errors-box screen-state)) field-id) '())
        '())))

;; Navigation and search result functions
(define RESULT-FETCH-BUFFER 10)

(define (ensure-selection-visible state visible-height)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let ([result-count (unbox (SearchResultsState-result-count-box screen-state))])
        (when result-count
          (let* ([selected-index (unbox (SearchResultsState-selected-index-box screen-state))]
                 [current-scroll (unbox (SearchResultsState-scroll-offset-box screen-state))]
                 [max-scroll (max 0 (- result-count visible-height))]
                 [new-scroll current-scroll])

            ;; Adjust scroll to keep selection visible with margins (1 above, 2 below)
            (when (< selected-index (+ current-scroll 1))
              (set! new-scroll (max 0 (- selected-index 1))))

            (when (or (> selected-index (- (+ current-scroll visible-height) 2))
                      (> (+ current-scroll visible-height) result-count))
              (set! new-scroll (min max-scroll (max 0 (- (+ selected-index 2) visible-height)))))

            ;; Update scroll and fetch new results window
            (set-box! (SearchResultsState-scroll-offset-box screen-state) new-scroll)
            (fetch-results-window state)))))))

(define (fetch-results-window state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let* ([engine (get-engine state)]
             [result-count (unbox (SearchResultsState-result-count-box screen-state))])
        (when result-count
          (let* ([scroll-offset (unbox (SearchResultsState-scroll-offset-box screen-state))]
                 [visible-height (unbox (SearchResultsState-content-height-box screen-state))]
                 [fetch-start scroll-offset]
                 [fetch-end (min (- result-count 1)
                                 (+ scroll-offset visible-height RESULT-FETCH-BUFFER))]
                 [results (if (and (>= result-count 0) (>= fetch-end fetch-start))
                              (Scooter-search-results-window engine fetch-start fetch-end)
                              '())])
            (set-box! (SearchResultsState-results-box screen-state) results)))))))

(define (navigate-by-amount state amount)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let ([result-count (unbox (SearchResultsState-result-count-box screen-state))])
        (when (and result-count (> result-count 0))
          (let* ([current-selected (unbox (SearchResultsState-selected-index-box screen-state))]
                 [new-selected (max 0 (min (- result-count 1) (+ current-selected amount)))])
            (set-box! (SearchResultsState-selected-index-box screen-state) new-selected)
            (ensure-selection-visible state
                                      (unbox (SearchResultsState-content-height-box
                                              screen-state)))))))))

(define (jump-to-top state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (set-box! (SearchResultsState-selected-index-box screen-state) 0)
      (fetch-results-window state))))

(define (jump-to-bottom state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let ([result-count (unbox (SearchResultsState-result-count-box screen-state))])
        (when (and result-count (> result-count 0))
          (set-box! (SearchResultsState-selected-index-box screen-state) (- result-count 1))
          (ensure-selection-visible state
                                    (unbox (SearchResultsState-content-height-box screen-state))))))))

(define (scroll-page state direction)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let ([results-height (unbox (SearchResultsState-content-height-box screen-state))])
        (navigate-by-amount state (* direction results-height))))))

(define (scroll-half-page state direction)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let ([results-height (unbox (SearchResultsState-content-height-box screen-state))])
        (navigate-by-amount state (* direction (max 1 (quotient results-height 2))))))))

(define (navigate-search-results state direction)
  (navigate-by-amount state direction))

(define (scroll-errors state direction)
  (let ([screen-state (get-current-screen state)])
    (when (SearchReplacementCompleteState? screen-state)
      (let ([engine (get-engine state)])
        (when (Scooter-replacement-complete? engine)
          (let* ([stats (Scooter-replacement-stats engine)]
                 [num-errors (ReplacementStats-num-errors stats)]
                 [current-offset (unbox (SearchReplacementCompleteState-error-scroll-offset-box
                                         screen-state))]
                 [new-offset (max 0 (+ current-offset direction))]
                 [max-offset (max 0 (- num-errors 1))])
            (when (> num-errors 0)
              (set-box! (SearchReplacementCompleteState-error-scroll-offset-box screen-state)
                        (min max-offset new-offset)))))))))

(define (toggle-search-result-inclusion state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let* ([selected-index (unbox (SearchResultsState-selected-index-box screen-state))]
             [engine (get-engine state)]
             [result-count (unbox (SearchResultsState-result-count-box screen-state))])
        (when (and result-count (> result-count 0))
          (Scooter-toggle-inclusion engine selected-index)
          (fetch-results-window state))))))

(define (toggle-all-search-results state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let* ([engine (get-engine state)]
             [result-count (unbox (SearchResultsState-result-count-box screen-state))])
        (when (and result-count (> result-count 0))
          (Scooter-toggle-all engine)
          (fetch-results-window state))))))

(define (open-selected-search-result state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let* ([selected-index (unbox (SearchResultsState-selected-index-box screen-state))]
             [engine (get-engine state)]
             [result-count (unbox (SearchResultsState-result-count-box screen-state))]
             [results (unbox (SearchResultsState-results-box screen-state))]
             [scroll-offset (unbox (SearchResultsState-scroll-offset-box screen-state))])
        (when (and result-count (> result-count 0) results)
          (let* ([local-index (- selected-index scroll-offset)]
                 [selected-result (list-ref results local-index)]
                 [file-path (SteelSearchResult-full-path selected-result)]
                 [line-num (SteelSearchResult-line-num selected-result)])
            (helix.open file-path)
            (helix.goto (int->string line-num))
            (align_view_center)))))))

;; Drawing functions
(define (preview-line-to-styled-segments line-segments)
  (map (lambda (segment)
         (let ([text (list-ref segment 0)]
               [fg-color (list-ref segment 1)]
               [bg-color (list-ref segment 2)])
           (cons text
                 (if (and (equal? fg-color "") (equal? bg-color ""))
                     (UIStyles-text (ui-styles))
                     (create-segment-style fg-color bg-color)))))
       line-segments))

(define (format-search-result result is-selected styles available-width)
  (let* ([prefix (if is-selected " > " "   ")]
         [checkbox (if (SteelSearchResult-included result) "[x]" "[ ]")]
         [line-num-str (int->string (SteelSearchResult-line-num result))]
         [raw-path (SteelSearchResult-display-path result)]

         [prefix-style (if is-selected
                           (UIStyles-selection styles)
                           (UIStyles-text styles))]
         [checkbox-style (UIStyles-info styles)]
         [base-text-style (if is-selected
                              (UIStyles-selection styles)
                              (UIStyles-text styles))]
         [text-style (if (SteelSearchResult-included result)
                         (style-with-bold base-text-style)
                         base-text-style)]
         [line-num-style (if is-selected
                             (UIStyles-selection styles)
                             (UIStyles-line-num styles))]

         [fixed-elements (list prefix checkbox " " ":" line-num-str)]
         [fixed-overhead (char-width-sum fixed-elements)]
         [max-path-width (max 10 (- available-width fixed-overhead))]
         [truncated-path (truncate-str-with-ellipsis raw-path max-path-width)]
         [parts (list (cons prefix prefix-style)
                      (cons checkbox checkbox-style)
                      (cons " " text-style)
                      (cons truncated-path text-style)
                      (cons ":" text-style)
                      (cons line-num-str line-num-style))]
         [remaining-width (- available-width (char-width-sum (map car parts)))])
    (append parts (list (cons (make-string remaining-width #\space) text-style)))))

(define (draw-file-preview frame preview-area result)
  (let* ([screen-height (area-height preview-area)]
         [screen-width (area-width preview-area)]
         [preview-lines (SteelSearchResult-build-preview result screen-height)]
         [bg-style (UIStyles-bg (ui-styles))])

    ;; Fill the preview area with the background color
    (buffer/clear frame preview-area)
    (let fill-background ([row 0])
      (when (< row screen-height)
        (frame-set-string! frame
                           (area-x preview-area)
                           (+ (area-y preview-area) row)
                           (make-space-string screen-width)
                           bg-style)
        (fill-background (+ row 1))))

    ;; Then render the text content on top
    (let loop ([lines preview-lines]
               [row 0])
      (when (and (not (null? lines)) (< row screen-height))
        (let* ([line-segments (car lines)]
               [styled-segments (preview-line-to-styled-segments line-segments)])
          (render-styled-segments-in-area frame preview-area row styled-segments bg-style)
          (loop (cdr lines) (+ row 1)))))))

(define (draw-search-fields frame content-area state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (let* ([general-error (unbox (SearchFieldsState-general-error-box screen-state))]
             [error-height 2]
             [all-fields (get-all-fields)]
             [field-count (length all-fields)]
             [total-fields-height (* field-count 3)] ; Each field is 3 rows high
             [available-height (- (area-height content-area) error-height)]
             [current-field (unbox (SearchFieldsState-current-field-box screen-state))]
             [centered-layout (calculate-centered-layout (area-x content-area)
                                                         (+ (area-y content-area) error-height)
                                                         (area-width content-area)
                                                         available-height
                                                         (area-width content-area)
                                                         total-fields-height
                                                         #f
                                                         #f
                                                         #f
                                                         #t)]
             [field-positions (calculate-field-positions (CenteredLayout-y centered-layout))])

        (let ([error-area (calculate-error-message-area content-area)])
          (draw-error-message frame error-area general-error))

        (let ([fields-area (area (area-x content-area)
                                 (CenteredLayout-y centered-layout)
                                 (area-width content-area)
                                 total-fields-height)])
          (draw-all-fields frame
                           fields-area
                           current-field
                           state
                           get-field-text
                           get-field-errors-safe))

        (position-cursor-in-text-field state
                                       current-field
                                       field-positions
                                       (area-x content-area)
                                       (area-width content-area))))))

(define (draw-search-results frame content-area state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (call-with-values
       (lambda () (calculate-split-areas content-area))
       (lambda (results-list-area preview-area)

         (set-box! (SearchResultsState-content-height-box screen-state)
                   (area-height results-list-area))
         (ensure-selection-visible state (area-height results-list-area))

         (let* ([styles (ui-styles)]
                [result-style (UIStyles-text styles)]
                [highlight-style (UIStyles-selection styles)]
                [result-count (or (unbox (SearchResultsState-result-count-box screen-state)) 0)]
                [is-complete (unbox (SearchResultsState-is-complete-box screen-state))]
                [results (or (unbox (SearchResultsState-results-box screen-state)) '())]
                [status-line (string-append "   "
                                            "Results: "
                                            (to-string result-count)
                                            " ["
                                            (if is-complete "Search complete" "Searching...")
                                            "]")]
                [selected-index (unbox (SearchResultsState-selected-index-box screen-state))]
                [scroll-offset (unbox (SearchResultsState-scroll-offset-box screen-state))]
                [status-area (calculate-status-area content-area)]
                [results-count (length results)])

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

           (let ([truncated-status (truncate-string status-line (- (area-width status-area) 4))])
             (frame-set-string! frame
                                (area-x status-area)
                                (area-y status-area)
                                truncated-status
                                (UIStyles-popup (ui-styles))))

           ;; Draw results list
           (when (> results-count 0)
             (let loop ([index 0]
                        [current-row 0])
               (when (and (< index results-count) (< current-row (area-height results-list-area)))
                 (let* ([result (list-ref results index)]
                        [absolute-index (+ index scroll-offset)]
                        [is-selected (= absolute-index selected-index)]
                        [styled-segments (format-search-result result
                                                               is-selected
                                                               styles
                                                               (area-width results-list-area))])
                   (render-styled-segments frame
                                           (area-x results-list-area)
                                           (+ (area-y results-list-area) current-row)
                                           styled-segments
                                           (area-width results-list-area)))
                 (loop (+ index 1) (+ current-row 1)))))

           ;; Draw preview for selected result
           (when (and (> results-count 0)
                      (>= selected-index scroll-offset)
                      (< selected-index (+ scroll-offset results-count)))
             (let* ([local-index (- selected-index scroll-offset)]
                    [selected-result (list-ref results local-index)])
               (draw-file-preview frame preview-area selected-result)))))))))

(define (draw-performing-replacement frame content-area state)
  (let* ([engine (get-engine state)]
         [num-completed (Scooter-num-replacements-complete engine)]
         [status-text
          (string-append "Performing replacement... " (int->string num-completed) " completed")]
         [text-style (UIStyles-text (ui-styles))]
         [y-pos (+ (area-y content-area) (quotient (area-height content-area) 2))]
         [x-pos (+ (area-x content-area)
                   (quotient (- (area-width content-area) (char-width status-text)) 2))])
    (frame-set-string! frame x-pos y-pos status-text text-style)))

(define (draw-replacement-complete frame content-area state)
  (let* ([screen-state (get-current-screen state)]
         [engine (get-engine state)]
         [stats (Scooter-replacement-stats engine)]
         [num-successes (ReplacementStats-num-successes stats)]
         [num-ignored (ReplacementStats-num-ignored stats)]
         [num-errors (ReplacementStats-num-errors stats)]
         [errors (if (> num-errors 0)
                     (Scooter-replacement-errors engine)
                     '())]
         [title-style (UIStyles-active (ui-styles))]
         [text-style (UIStyles-text (ui-styles))]
         [error-style (UIStyles-error (ui-styles))]
         [start-y (+ (area-y content-area) (quotient (area-height content-area) 3))]
         [center-x (+ (area-x content-area) (quotient (area-width content-area) 2))])

    (let ([title "Replacement complete!"]
          [subtitle "Press <enter> to exit"]
          [title-y start-y])
      (for-each (lambda (pair)
                  (let ([offset (car pair)]
                        [str (cadr pair)])
                    (frame-set-string! frame
                                       (+ (area-x content-area)
                                          (quotient (- (area-width content-area) (char-width str)) 2))
                                       (+ title-y offset)
                                       str
                                       title-style)))
                (list (list 0 title) (list 1 subtitle))))
    (let* ([content-x (area-x content-area)]
           [content-width (area-width content-area)]
           [box-width (min (- content-width 20) 76)] ; Leave some margin, max width like example
           [box-x (+ content-x (quotient (- content-width box-width) 2))]
           [stats-data (list (list "Successful replacements (lines):" (int->string num-successes))
                             (list "Ignored (lines):" (int->string num-ignored))
                             (list "Errors:" (int->string num-errors)))]
           [stats-y (+ start-y 3)])

      (let loop ([stats-list stats-data]
                 [current-y stats-y])
        (when (not (null? stats-list))
          (let* ([stat-data (car stats-list)]
                 [label (car stat-data)]
                 [value (cadr stat-data)]
                 [box-area (area box-x current-y box-width 3)])

            (block/render frame box-area (make-block text-style text-style "all" "plain"))
            (frame-set-string! frame (+ box-x 1) current-y label text-style)
            (frame-set-string! frame (+ box-x 1) (+ current-y 1) value text-style)
            (if (null? (cdr stats-list))
                (when (> num-errors 0)
                  (let* ([errors-y (+ current-y 5)]
                         [error-box-x (area-x content-area)]
                         [error-box-width (area-width content-area)]
                         [scroll-offset (unbox (SearchReplacementCompleteState-error-scroll-offset-box
                                                screen-state))]
                         [visible-errors (drop errors scroll-offset)])
                    (frame-set-string! frame (+ error-box-x 2) errors-y "Errors:" text-style)
                    (let render-errors ([error-list visible-errors]
                                        [error-y (+ errors-y 2)])
                      (when (and (not (null? error-list))
                                 (< error-y (+ (area-y content-area) (area-height content-area) -2)))
                        (let* ([error-result (car error-list)]
                               [error-info (SteelSearchResult-display-error error-result)]
                               [path-display (car error-info)]
                               [error-msg (cadr error-info)]
                               [truncated-path (truncate-string (string-append path-display ":")
                                                                (- error-box-width 4))]
                               [truncated-error (truncate-string error-msg (- error-box-width 6))])
                          ;; Render path on first line
                          (frame-set-string! frame
                                             (+ error-box-x 4)
                                             error-y
                                             truncated-path
                                             text-style)
                          ;; Render error message on second line, indented
                          (when (< (+ error-y 1)
                                   (+ (area-y content-area) (area-height content-area) -2))
                            (frame-set-string! frame
                                               (+ error-box-x 6)
                                               (+ error-y 1)
                                               truncated-error
                                               error-style))
                          (render-errors (cdr error-list) (+ error-y 3)))))))
                (loop (cdr stats-list) (+ current-y 3)))))))))

(define (clear-errors-on-input! state field-id)
  (clear-field-error! state field-id)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (set-box! (SearchFieldsState-general-error-box screen-state) #f))))

(define (handle-textfield-key textfield event)
  (cond
    [(or (and (key-event-char event)
              (equal? (key-event-char event) #\w)
              (equal? (key-event-modifier event) key-modifier-ctrl))
         (and (key-event-backspace? event) (equal? (key-event-modifier event) key-modifier-alt)))
     (TextField-delete-word-backward textfield)]

    [(or (and (key-event-char event)
              (equal? (key-event-char event) #\u)
              (equal? (key-event-modifier event) key-modifier-ctrl))
         (and (key-event-backspace? event) (= (key-event-modifier event) key-modifier-super)))
     (TextField-delete-to-start textfield)]

    [(key-event-backspace? event) (TextField-delete-char textfield)]

    [(or (and (key-event-char event)
              (or (equal? (key-event-char event) #\b) (equal? (key-event-char event) #\B))
              (equal? (key-event-modifier event) key-modifier-alt))
         (and (key-event-left? event) (equal? (key-event-modifier event) key-modifier-alt)))
     (TextField-move-cursor-back-word textfield)]

    [(or (and (key-event-char event)
              (equal? (key-event-char event) #\a)
              (equal? (key-event-modifier event) key-modifier-ctrl))
         (and (key-event-left? event) (= (key-event-modifier event) key-modifier-super))
         (key-event-home? event))
     (TextField-move-cursor-start textfield)]

    [(key-event-left? event) (TextField-move-cursor-left textfield)]

    [(or (and (key-event-char event)
              (or (equal? (key-event-char event) #\f) (equal? (key-event-char event) #\F))
              (equal? (key-event-modifier event) key-modifier-alt))
         (and (key-event-right? event) (equal? (key-event-modifier event) key-modifier-alt)))
     (TextField-move-cursor-forward-word textfield)]

    [(or (and (key-event-char event)
              (equal? (key-event-char event) #\e)
              (equal? (key-event-modifier event) key-modifier-ctrl))
         (and (key-event-right? event) (= (key-event-modifier event) key-modifier-super))
         (key-event-end? event))
     (TextField-move-cursor-end textfield)]

    [(key-event-right? event) (TextField-move-cursor-right textfield)]

    [(or (and (key-event-char event)
              (equal? (key-event-char event) #\d)
              (equal? (key-event-modifier event) key-modifier-alt))
         (and (key-event-delete? event) (equal? (key-event-modifier event) key-modifier-alt)))
     (TextField-delete-word-forward textfield)]

    [(or (and (key-event-char event)
              (equal? (key-event-char event) #\d)
              (equal? (key-event-modifier event) key-modifier-ctrl))
         (key-event-delete? event))
     (TextField-delete-char-forward textfield)]

    [(key-event-char event) (TextField-enter-char textfield (key-event-char event))]

    [else #f]))

(define (insert-text state field-id text)
  (let ([screen-state (get-current-screen state)])
    (when (and (SearchFieldsState? screen-state) (field-is-text? field-id))
      (clear-errors-on-input! state field-id)
      (let ([textfield (get-field-value-internal screen-state field-id)])
        (when (TextField? textfield)
          (TextField-insert-text textfield text))))))

(define (position-cursor-in-text-field state current-field field-positions content-x content-width)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (let ([active-field-def (get-field-by-id current-field)])
        (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
          (let* ([positions (hash-ref field-positions current-field)]
                 [box-top-y (car positions)]
                 [textfield (get-field-value-internal screen-state current-field)]
                 [cursor-pos (if (TextField? textfield)
                                 (TextField-cursor-pos textfield)
                                 0)]
                 [cursor-row (+ box-top-y 1)]
                 [cursor-col (get-field-cursor-column content-x content-width cursor-pos)])
            (set-position-row! (ScooterWindow-cursor-position state) cursor-row)
            (set-position-col! (ScooterWindow-cursor-position state) cursor-col)))))))

(define (strip-newlines text)
  (string-replace (string-replace text "\n" " ") "\r" " "))

(define (calculate-error-message-area content-area)
  (area (area-x content-area) (area-y content-area) (area-width content-area) 1))

(define (draw-error-message frame error-area error-message)
  (when error-message
    (let* ([error-style (UIStyles-error (ui-styles))]
           [clean-message (strip-newlines error-message)]
           [error-text (string-append "Error: " clean-message)]
           [truncated-error (truncate-string error-text (area-width error-area))])
      (frame-set-string! frame (area-x error-area) (area-y error-area) truncated-error error-style))))

(define (format-keybinding key action)
  (string-append "<" key "> " action))

(define (get-keybinding-help screen-state)
  (let* ([common-bindings '(("ctrl+r" "reset") ("esc" "hide") ("ctrl+c" "quit"))]
         [mode-specific-bindings
          (cond
            [(SearchFieldsState? screen-state)
             '(("enter" "search") ("tab" "next field") ("space" "toggle"))]
            [(SearchResultsState? screen-state)
             '(("enter" "replace") ("space" "toggle")
                                   ("a" "toggle all")
                                   ("e" "open")
                                   ; ("alt+e" "open bg") ; TODO: show in popup
                                   ("ctrl+o" "back"))]
            [(SearchPerformingReplacementState? screen-state) '()]
            [(SearchReplacementCompleteState? screen-state) '(("enter" "quit"))]
            [else '()])]
         [all-bindings (append mode-specific-bindings common-bindings)]
         [formatted-bindings (map (lambda (binding) (format-keybinding (car binding) (cadr binding)))
                                  all-bindings)])
    (string-join formatted-bindings " / ")))

(define (calculate-title-area window-area)
  (area (+ (area-x window-area) 2) (area-y window-area) (- (area-width window-area) 4) 1))

(define (draw-title frame title-area title)
  (let ([popup-style (UIStyles-popup (ui-styles))])
    (frame-set-string! frame (area-x title-area) (area-y title-area) title popup-style)))

(define (calculate-keybinding-help-area content-area)
  (area (area-x content-area)
        (+ (area-y content-area) (area-height content-area) 1)
        (area-width content-area)
        1))

(define (draw-keybinding-help frame help-area screen-state)
  (let* ([hint-style (UIStyles-dim (ui-styles))]
         [hint-text (get-keybinding-help screen-state)]
         [truncated-hint (truncate-string hint-text (area-width help-area))]
         [text-length (char-width truncated-hint)]
         [available-width (area-width help-area)]
         [padding (max 0 (quotient (- available-width text-length) 2))]
         [centered-x (+ (area-x help-area) padding)])
    (when (> (char-width hint-text) 0)
      (frame-set-string! frame centered-x (area-y help-area) truncated-hint hint-style))))

(define (scooter-render state rect frame)
  (let* ([window-area (calculate-window-area rect)]
         [content-area (calculate-content-area window-area)]
         [screen-state (get-current-screen state)]
         [title " Scooter "]
         [popup-style (UIStyles-popup (ui-styles))])

    (buffer/clear frame window-area)
    (block/render frame window-area (make-block popup-style popup-style "all" "plain"))
    (let ([title-area (calculate-title-area window-area)]) (draw-title frame title-area title))

    (cond
      [(SearchFieldsState? screen-state) (draw-search-fields frame content-area state)]

      [(SearchResultsState? screen-state) (draw-search-results frame content-area state)]

      [(SearchPerformingReplacementState? screen-state)
       (draw-performing-replacement frame content-area state)]

      [(SearchReplacementCompleteState? screen-state)
       (draw-replacement-complete frame content-area state)])

    (let ([help-area (calculate-keybinding-help-area content-area)])
      (draw-keybinding-help frame help-area screen-state))))

(define (handle-paste-event state paste-text)
  (let ([screen-state (get-current-screen state)])
    (when (and paste-text (SearchFieldsState? screen-state))
      (let* ([current-field-id (unbox (SearchFieldsState-current-field-box screen-state))]
             [clean-text (strip-newlines paste-text)])
        (when (field-is-text? current-field-id)
          (insert-text state current-field-id clean-text))))))

(define (handle-tab-key state modifier)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (let* ([current-field-id (unbox (SearchFieldsState-current-field-box screen-state))]
             [new-field-id (if (equal? modifier key-modifier-shift)
                               (get-previous-field current-field-id)
                               (get-next-field current-field-id))])
        (set-box! (SearchFieldsState-current-field-box screen-state) new-field-id)))))

(define (handle-enter-key state)
  (let ([search-term (get-field-text state 'search)])
    (if (> (char-width search-term) 0)
        (start-scooter-search state)
        (let ([screen-state (get-current-screen state)])
          (when (SearchFieldsState? screen-state)
            (set-field-errors-internal! screen-state 'search '("Search text is required")))))))

(define (handle-search-fields-mode-event state event)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (cond
        [(paste-event? event) (handle-paste-event state (paste-event-string event))]
        [(key-event-tab? event) (handle-tab-key state (key-event-modifier event))]
        [(key-event-enter? event) (handle-enter-key state)]
        [else
         (let* ([current-field-id (unbox (SearchFieldsState-current-field-box screen-state))]
                [field-def (get-field-by-id current-field-id)])
           (when (and field-def (equal? (field-type field-def) FIELD-TYPE-TEXT))
             (clear-errors-on-input! state current-field-id)
             (let ([textfield (get-field-value-internal screen-state current-field-id)])
               (when (TextField? textfield)
                 (handle-textfield-key textfield event))))

           (when (and field-def
                      (equal? (field-type field-def) FIELD-TYPE-BOOLEAN)
                      (key-event-char event)
                      (equal? (key-event-char event) #\space))
             (let ([current-value (get-field-value-internal screen-state current-field-id)])
               (set-field-value-internal! screen-state current-field-id (not current-value))
               (when (equal? current-field-id 'fixed-strings)
                 (clear-field-error! state 'search)))))]))
    event-result/consume))

(define (key-matches-char? event char)
  (and (key-event-char event) (equal? (key-event-char event) char)))

(define (key-with-ctrl? event char)
  (and (key-matches-char? event char) (equal? (key-event-modifier event) key-modifier-ctrl)))

(define (start-replacement state)
  (let ([engine (get-engine state)])
    (Scooter-start-replace engine)
    (set-current-screen! state (SearchPerformingReplacementState))
    (poll-replacement-progress state)))

(define (handle-performing-replacement-event state event)
  event-result/consume)

(define (handle-replacement-complete-event state event)
  (cond
    [(key-event-enter? event) 'destroy-and-close]
    [(or (key-event-up? event) (key-matches-char? event #\k)) (scroll-errors state -1)]
    [(or (key-event-down? event) (key-matches-char? event #\j)) (scroll-errors state 1)]
    [else event-result/consume]))

(define (handle-search-results-mode-event state event)
  (cond
    [(key-with-ctrl? event #\o)
     (cancel-search-and-return-to-fields state)
     event-result/consume]
    [(or (key-event-up? event) (key-matches-char? event #\k))
     (navigate-search-results state -1)
     event-result/consume]
    [(or (key-event-down? event) (key-matches-char? event #\j))
     (navigate-search-results state 1)
     event-result/consume]
    [(key-with-ctrl? event #\u)
     (scroll-half-page state -1)
     event-result/consume]
    [(key-with-ctrl? event #\d)
     (scroll-half-page state 1)
     event-result/consume]
    [(key-event-page-up? event)
     (scroll-page state -1)
     event-result/consume]
    [(key-event-page-down? event)
     (scroll-page state 1)
     event-result/consume]
    [(key-matches-char? event #\g)
     (jump-to-top state)
     event-result/consume]
    [(key-matches-char? event #\G)
     (jump-to-bottom state)
     event-result/consume]
    [(key-matches-char? event #\space)
     (toggle-search-result-inclusion state)
     event-result/consume]
    [(key-matches-char? event #\a)
     (toggle-all-search-results state)
     event-result/consume]
    [(key-matches-char? event #\e)
     (open-selected-search-result state)
     (if (equal? (key-event-modifier event) key-modifier-alt)
         event-result/consume
         event-result/close)]
    [(key-event-enter? event)
     (let ([engine (get-engine state)])
       (when (Scooter-search-complete? engine)
         (start-replacement state))
       event-result/consume)]
    [else event-result/consume]))

(define (scooter-event-handler state event)
  (let ([screen-state (get-current-screen state)])
    (cond
      [(key-with-ctrl? event #\r)
       (reset-scooter-state! state)
       event-result/consume]
      [(SearchFieldsState? screen-state) (handle-search-fields-mode-event state event)]
      [(SearchResultsState? screen-state) (handle-search-results-mode-event state event)]
      [(SearchPerformingReplacementState? screen-state)
       (handle-performing-replacement-event state event)]
      [(SearchReplacementCompleteState? screen-state) (handle-replacement-complete-event state event)]
      [else event-result/consume])))

(define (scooter-cursor-handler state _)
  (let ([screen-state (get-current-screen state)])
    (and (SearchFieldsState? screen-state)
         (field-is-text? (unbox (SearchFieldsState-current-field-box screen-state)))
         (ScooterWindow-cursor-position state))))

(define (start-scooter-search state)
  (clear-all-errors! state)
  (execute-search-process! state))

(define (cancel-search-and-return-to-fields state)
  (let ([engine (get-engine state)]
        [preserved-fields (let ([rs-state (get-current-screen state)])
                            (if (SearchResultsState? rs-state)
                                (SearchResultsState-preserved-fields-state rs-state)
                                #f))])
    (Scooter-cancel-search engine)
    (if preserved-fields
        (set-current-screen! state preserved-fields)
        (set-current-screen! state (create-default-search-fields-state)))
    (clear-all-errors! state)))

(define (execute-search-process! state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      (let* ([engine (get-engine state)]
             [search-term (get-field-text state 'search)]
             [replace-term (get-field-text state 'replace)]
             [fixed-strings (get-field-value-internal screen-state 'fixed-strings)]
             [match-whole-word (get-field-value-internal screen-state 'match-whole-word)]
             [match-case (get-field-value-internal screen-state 'match-case)]
             [include-pattern (get-field-text state 'files-include)]
             [exclude-pattern (get-field-text state 'files-exclude)]
             [response (Scooter-start-search engine
                                             search-term
                                             replace-term
                                             fixed-strings
                                             match-whole-word
                                             match-case
                                             include-pattern
                                             exclude-pattern)])

        (if (hash-ref response "success")
            (begin
              (let ([current-fields-state (get-current-screen state)])
                (set-current-screen! state
                                     (create-default-search-results-state current-fields-state)))
              (poll-search-results state))
            (handle-search-errors! state response))))))

(define (handle-search-errors! state response)
  (let ([error-type (hash-try-get response "error-type")]
        [message (hash-try-get response "message")])
    (cond
      [(equal? error-type "validation-error") (handle-validation-errors! state response)]
      [(equal? error-type "configuration-error")
       (let ([screen-state (get-current-screen state)])
         (when (SearchFieldsState? screen-state)
           (set-box! (SearchFieldsState-general-error-box screen-state)
                     (or message "Configuration error occurred"))))]
      [error-type
       (let ([screen-state (get-current-screen state)])
         (when (SearchFieldsState? screen-state)
           (set-box! (SearchFieldsState-general-error-box screen-state)
                     (string-append "Error (" error-type "): " (or message "Unknown error")))))]
      [else
       (let ([screen-state (get-current-screen state)])
         (when (SearchFieldsState? screen-state)
           (set-box! (SearchFieldsState-general-error-box screen-state)
                     "An unexpected error occurred")))])))

(define (handle-validation-errors! state response)
  (let ([screen-state (get-current-screen state)])
    (when (SearchFieldsState? screen-state)
      ;; Map validation error response keys to field IDs
      (when (hash-contains? response "search-text-errors")
        (set-field-errors-internal! screen-state 'search (hash-ref response "search-text-errors")))
      (when (hash-contains? response "include-files-errors")
        (set-field-errors-internal! screen-state
                                    'files-include
                                    (hash-ref response "include-files-errors")))
      (when (hash-contains? response "exclude-files-errors")
        (set-field-errors-internal! screen-state
                                    'files-exclude
                                    (hash-ref response "exclude-files-errors"))))))

(define (poll-replacement-progress state)
  (let ([engine (get-engine state)])
    (enqueue-thread-local-callback
     (lambda ()
       (let ([is-complete (Scooter-replacement-complete? engine)])
         (cond
           [is-complete (set-current-screen! state (create-default-replacement-complete-state))]
           ;; Only continue polling if still on replacement screen
           [(SearchPerformingReplacementState? (get-current-screen state))
            (enqueue-thread-local-callback (lambda () (poll-replacement-progress state)))]))))))

(define (poll-search-results state)
  (let ([screen-state (get-current-screen state)])
    (when (SearchResultsState? screen-state)
      (let ([engine (get-engine state)])
        (enqueue-thread-local-callback
         (lambda ()
           (let ([result-count (Scooter-search-result-count engine)]
                 [is-complete (Scooter-search-complete? engine)])

             (set-box! (SearchResultsState-result-count-box screen-state) result-count)
             (set-box! (SearchResultsState-is-complete-box screen-state) is-complete)
             (fetch-results-window state)

             (when (not is-complete)
               (enqueue-thread-local-callback (lambda () (poll-search-results state)))))))))))
