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
         start-scooter-search
         get-lines
         get-selected-index
         set-selected-index!
         get-scroll-offset
         set-scroll-offset!
         get-content-height
         set-content-height!
         get-engine
         ScooterWindow-lines-box
         SearchData
         get-search-data
         reset-scooter-state!
         cancel-all-operations!)

(define WINDOW-SIZE-RATIO 0.9)
(define CONTENT-PADDING 2)
(define SCROLL-MARGIN 2)
(define STATUS-HEIGHT 2)

(struct SearchData (result-count is-complete results scroll-offset))

(struct ScooterWindow
        (current-screen-box ; 'search-fields, 'search-results, 'performing-replacement, 'replacement-complete
         field-values-box
         cursor-positions-box
         current-field-box
         lines-box
         completed-box
         cursor-position
         engine-box
         selected-index-box
         scroll-offset-box
         content-height-box
         field-errors-box
         general-error-box
         error-scroll-offset-box))

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
(define-hash-accessors get-field-errors set-field-errors! ScooterWindow-field-errors-box)

(define (get-field-textfield state field-id)
  (hash-ref (unbox (ScooterWindow-field-values-box state)) field-id))

(define (get-field-text state field-id)
  (let ([field-value (get-field-value state field-id)])
    (if (TextField? field-value)
        (TextField-text field-value)
        field-value)))

(define (get-current-screen state)
  (unbox (ScooterWindow-current-screen-box state)))
(define (set-current-screen! state value)
  (set-box! (ScooterWindow-current-screen-box state) value))

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

(define (get-general-error state)
  (unbox (ScooterWindow-general-error-box state)))
(define (set-general-error! state value)
  (set-box! (ScooterWindow-general-error-box state) value))
(define (clear-general-error! state)
  (set-box! (ScooterWindow-general-error-box state) #f))

(define (get-error-scroll-offset state)
  (unbox (ScooterWindow-error-scroll-offset-box state)))
(define (set-error-scroll-offset! state value)
  (set-box! (ScooterWindow-error-scroll-offset-box state) value))

(define (get-search-data state)
  (let ([lines (get-lines state)]) (and (SearchData? lines) lines)))

(define (clear-all-errors! state)
  (clear-all-field-errors! state)
  (clear-general-error! state))

; TODO: tidy this up
(define (create-scooter-window)
  (let ([directory (get-helix-cwd)])
    (ScooterWindow (box 'search-fields) ; current-screen-box
                   (box (create-initial-field-values)) ; field-values-box
                   (box (create-initial-cursor-positions)) ; cursor-positions-box
                   (box 'search) ; current-field-box
                   (box (SearchData 0 #f '() 0)) ; lines-box
                   (box #f) ; completed-box
                   (position 0 0) ; cursor-position
                   (box (Scooter-new directory #f)) ; engine-box
                   (box 0) ; selected-index-box
                   (box 0) ; scroll-offset-box
                   (box 10) ; content-height-box
                   (box (hash)) ; field-errors-box
                   (box #f) ; general-error-box
                   (box 0)))) ; error-scroll-offset-box

(define (reset-scooter-state! state)
  (cancel-all-operations! state)
  (let* ([engine (get-engine state)]
         [default-state (create-scooter-window)]
         [box-accessors (list ScooterWindow-current-screen-box
                              ScooterWindow-field-values-box
                              ScooterWindow-cursor-positions-box
                              ScooterWindow-current-field-box
                              ScooterWindow-lines-box
                              ScooterWindow-completed-box
                              ScooterWindow-selected-index-box
                              ScooterWindow-scroll-offset-box
                              ScooterWindow-field-errors-box
                              ScooterWindow-general-error-box
                              ScooterWindow-error-scroll-offset-box)])
    (Scooter-reset engine)
    (for-each (lambda (accessor) (set-box! (accessor state) (unbox (accessor default-state))))
              box-accessors)))

(define (cancel-all-operations! state)
  (let ([engine (get-engine state)])
    (Scooter-cancel-search engine)
    (Scooter-cancel-replacement engine)))

(define (clear-all-field-errors! state)
  (set-box! (ScooterWindow-field-errors-box state) (hash)))

(define (clear-field-error! state field-id)
  (let ([errors (unbox (ScooterWindow-field-errors-box state))])
    (when (hash-contains? errors field-id)
      (set-box! (ScooterWindow-field-errors-box state) (hash-remove errors field-id)))))

(define (get-field-errors-safe state field-id)
  (or (hash-try-get (unbox (ScooterWindow-field-errors-box state)) field-id) '()))

;; Navigation and search result functions
(define RESULT-FETCH-BUFFER 10)

(define (ensure-selection-visible state visible-height)
  (let ([data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)]
             [selected-index (get-selected-index state)]
             [current-scroll (get-scroll-offset state)]
             [max-scroll (max 0 (- result-count visible-height))]
             [new-scroll current-scroll])

        ;; Adjust scroll to keep selection visible with margins (1 above, 2 below)
        (when (< selected-index (+ current-scroll 1))
          (set! new-scroll (max 0 (- selected-index 1))))

        (when (or (> selected-index (- (+ current-scroll visible-height) 2))
                  (> (+ current-scroll visible-height) result-count))
          (set! new-scroll (min max-scroll (max 0 (- (+ selected-index 2) visible-height)))))

        ;; Update scroll and fetch new results window
        (set-scroll-offset! state new-scroll)
        (fetch-results-window state)))))

(define (fetch-results-window state)
  (let* ([engine (get-engine state)]
         [data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)]
             [is-complete (SearchData-is-complete data)]
             [scroll-offset (get-scroll-offset state)]
             [visible-height (get-content-height state)]
             [fetch-start scroll-offset]
             [fetch-end (min (- result-count 1) (+ scroll-offset visible-height RESULT-FETCH-BUFFER))]
             [results (if (and (>= result-count 0) (>= fetch-end fetch-start))
                          (Scooter-search-results-window engine fetch-start fetch-end)
                          '())])
        (set-box! (ScooterWindow-lines-box state)
                  (SearchData result-count is-complete results scroll-offset))))))

(define (navigate-by-amount state amount)
  (let ([data (get-search-data state)])
    (when data
      (let* ([result-count (SearchData-result-count data)]
             [current-selected (get-selected-index state)]
             [new-selected (max 0 (min (- result-count 1) (+ current-selected amount)))])
        (when (> result-count 0)
          (set-selected-index! state new-selected)
          (ensure-selection-visible state (get-content-height state)))))))

(define (jump-to-top state)
  (set-selected-index! state 0)
  (fetch-results-window state))

(define (jump-to-bottom state)
  (let ([data (get-search-data state)])
    (when data
      (let ([result-count (SearchData-result-count data)])
        (when (> result-count 0)
          (set-selected-index! state (- result-count 1))
          (ensure-selection-visible state (get-content-height state)))))))

(define (scroll-page state direction)
  (let ([results-height (get-content-height state)])
    (navigate-by-amount state (* direction results-height))))

(define (scroll-half-page state direction)
  (let ([results-height (get-content-height state)])
    (navigate-by-amount state (* direction (max 1 (quotient results-height 2))))))

(define (navigate-search-results state direction)
  (navigate-by-amount state direction))

(define (scroll-errors state direction)
  (let ([engine (get-engine state)])
    (when (Scooter-replacement-complete? engine)
      (let* ([stats (Scooter-replacement-stats engine)]
             [num-errors (ReplacementStats-num-errors stats)]
             [current-offset (get-error-scroll-offset state)]
             [new-offset (max 0 (+ current-offset direction))]
             [max-offset (max 0 (- num-errors 1))])
        (when (> num-errors 0)
          (set-error-scroll-offset! state (min max-offset new-offset)))))))

(define (toggle-search-result-inclusion state)
  (let* ([selected-index (get-selected-index state)]
         [engine (get-engine state)]
         [data (get-search-data state)])
    (when (> (SearchData-result-count data) 0)
      (Scooter-toggle-inclusion engine selected-index)
      (fetch-results-window state))))

(define (toggle-all-search-results state)
  (let* ([engine (get-engine state)]
         [data (get-search-data state)])
    (when (and data (> (SearchData-result-count data) 0))
      (Scooter-toggle-all engine)
      (fetch-results-window state))))

(define (open-selected-search-result state)
  (let* ([selected-index (get-selected-index state)]
         [engine (get-engine state)]
         [data (get-search-data state)])
    (when (> (SearchData-result-count data) 0)
      (let* ([results (SearchData-results data)]
             [local-index (- selected-index (SearchData-scroll-offset data))]
             [selected-result (list-ref results local-index)]
             [file-path (SteelSearchResult-full-path selected-result)]
             [line-num (SteelSearchResult-line-num selected-result)])
        (helix.open file-path)
        (helix.goto (int->string line-num))
        (align_view_center)))))

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

(define (draw-search-results frame content-area initial-data state)
  (call-with-values
   (lambda () (calculate-split-areas content-area))
   (lambda (results-list-area preview-area)

     (set-content-height! state (area-height results-list-area))
     (ensure-selection-visible state (area-height results-list-area))

     (let* ([raw-data (or (get-lines state) initial-data)]
            [styles (ui-styles)]
            [result-style (UIStyles-text styles)]
            [highlight-style (UIStyles-selection styles)]
            [result-count (SearchData-result-count raw-data)]
            [is-complete (SearchData-is-complete raw-data)]
            [results (SearchData-results raw-data)]
            [data-scroll-offset (SearchData-scroll-offset raw-data)]
            [status-line (string-append "   "
                                        "Results: "
                                        (to-string result-count)
                                        " ["
                                        (if is-complete "Search complete" "Searching...")
                                        "]")]
            [selected-index (get-selected-index state)]
            [scroll-offset (get-scroll-offset state)]
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
                    [absolute-index (+ index data-scroll-offset)]
                    [is-selected (= absolute-index selected-index)]
                    [styled-segments
                     (format-search-result result is-selected styles (area-width results-list-area))])
               (render-styled-segments frame
                                       (area-x results-list-area)
                                       (+ (area-y results-list-area) current-row)
                                       styled-segments
                                       (area-width results-list-area)))
             (loop (+ index 1) (+ current-row 1)))))

       ;; Draw preview for selected result
       (when (and (> results-count 0)
                  (>= selected-index data-scroll-offset)
                  (< selected-index (+ data-scroll-offset results-count)))
         (let* ([local-index (- selected-index data-scroll-offset)]
                [selected-result (list-ref results local-index)])
           (draw-file-preview frame preview-area selected-result)))))))

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
  (let* ([engine (get-engine state)]
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
                         [scroll-offset (get-error-scroll-offset state)]
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
  (clear-general-error! state))

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
         ; TODO: uncomment when https://github.com/mattwparas/helix/pull/46 is merged
         ; (and (key-event-backspace? event) (= (key-event-modifier event) key-modifier-super))
         )
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
         ; TODO: uncomment when https://github.com/mattwparas/helix/pull/46 is merged
         ; (and (key-event-left? event) (= (key-event-modifier event) key-modifier-super))
         )
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
         ; TODO: uncomment when https://github.com/mattwparas/helix/pull/46 is merged
         ; (and (key-event-right? event) (= (key-event-modifier event) key-modifier-super))
         )
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
  (when (field-is-text? field-id)
    (clear-errors-on-input! state field-id)
    (let ([textfield (get-field-value state field-id)])
      (when (TextField? textfield)
        (TextField-insert-text textfield text)))))

(define (position-cursor-in-text-field state current-field field-positions content-x content-width)
  (let ([active-field-def (get-field-by-id current-field)])
    (when (and active-field-def (equal? (field-type active-field-def) FIELD-TYPE-TEXT))
      (let* ([positions (hash-ref field-positions current-field)]
             [box-top-y (car positions)]
             [textfield (get-field-value state current-field)]
             [cursor-pos (if (TextField? textfield)
                             (TextField-cursor-pos textfield)
                             0)]
             [cursor-row (+ box-top-y 1)]
             [cursor-col (get-field-cursor-column content-x content-width cursor-pos)])
        (set-position-row! (ScooterWindow-cursor-position state) cursor-row)
        (set-position-col! (ScooterWindow-cursor-position state) cursor-col)))))

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

(define (get-keybinding-help mode)
  (let* ([common-bindings '(("ctrl+r" "reset") ("esc" "hide") ("ctrl+c" "quit"))]
         [mode-specific-bindings (cond
                                   [(equal? mode 'search-fields)
                                    '(("enter" "search") ("tab" "next field") ("space" "toggle"))]
                                   [(equal? mode 'search-results)
                                    '(("enter" "replace") ("space" "toggle")
                                                          ("a" "toggle all")
                                                          ("e" "open")
                                                          ; ("alt+e" "open bg") ; TODO: show in popup
                                                          ("ctrl+o" "back"))]
                                   [(equal? mode 'performing-replacement) '()]
                                   [(equal? mode 'replacement-complete) '(("enter" "quit"))]
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

(define (draw-keybinding-help frame help-area mode)
  (let* ([hint-style (UIStyles-dim (ui-styles))]
         [hint-text (get-keybinding-help mode)]
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
         [mode (get-current-screen state)]
         [search-term (get-field-text state 'search)]
         [current-field (get-current-field state)]
         [title " Scooter "]
         [popup-style (UIStyles-popup (ui-styles))])

    (buffer/clear frame window-area)
    (block/render frame window-area (make-block popup-style popup-style "all" "plain"))
    (let ([title-area (calculate-title-area window-area)]) (draw-title frame title-area title))

    (cond
      [(equal? mode 'search-fields)
       (let* ([general-error (get-general-error state)]
              [error-height 2]
              [all-fields (get-all-fields)]
              [field-count (length all-fields)]
              [total-fields-height (* field-count 3)] ; Each field is 3 rows high
              [available-height (- (area-height content-area) error-height)]
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
                                        (area-width content-area)))]

      [(equal? mode 'search-results)
       (let ([lines (get-lines state)]) (draw-search-results frame content-area lines state))]

      [(equal? mode 'performing-replacement) (draw-performing-replacement frame content-area state)]

      [(equal? mode 'replacement-complete) (draw-replacement-complete frame content-area state)])

    (let ([help-area (calculate-keybinding-help-area content-area)])
      (draw-keybinding-help frame help-area mode))))

(define (handle-paste-event state paste-text)
  (when paste-text
    (let* ([current-field-id (get-current-field state)]
           [clean-text (strip-newlines paste-text)])
      (when (field-is-text? current-field-id)
        (insert-text state current-field-id clean-text)))))

(define (handle-tab-key state modifier)
  (let* ([current-field-id (get-current-field state)]
         [new-field-id (if (equal? modifier key-modifier-shift)
                           (get-previous-field current-field-id)
                           (get-next-field current-field-id))])
    (set-current-field! state new-field-id)))

(define (handle-enter-key state)
  (let ([search-term (get-field-text state 'search)])
    (if (> (char-width search-term) 0)
        (start-scooter-search state)
        (set-field-errors! state 'search '("Search text is required")))))

(define (handle-search-fields-mode-event state event)
  (cond
    [(paste-event? event) (handle-paste-event state (paste-event-string event))]
    [(key-event-tab? event) (handle-tab-key state (key-event-modifier event))]
    [(key-event-enter? event) (handle-enter-key state)]
    [else
     (let* ([current-field-id (get-current-field state)]
            [field-def (get-field-by-id current-field-id)])
       (when (and field-def (equal? (field-type field-def) FIELD-TYPE-TEXT))
         (clear-errors-on-input! state current-field-id)
         (let ([textfield (get-field-value state current-field-id)])
           (when (TextField? textfield)
             (handle-textfield-key textfield event))))

       (when (and field-def
                  (equal? (field-type field-def) FIELD-TYPE-BOOLEAN)
                  (key-event-char event)
                  (equal? (key-event-char event) #\space))
         (let ([current-value (get-field-value state current-field-id)])
           (set-field-value! state current-field-id (not current-value))
           (when (equal? current-field-id 'fixed-strings)
             (clear-field-error! state 'search)))))])
  event-result/consume)

(define (key-matches-char? event char)
  (and (key-event-char event) (equal? (key-event-char event) char)))

(define (key-with-ctrl? event char)
  (and (key-matches-char? event char) (equal? (key-event-modifier event) key-modifier-ctrl)))

(define (start-replacement state)
  (let ([engine (get-engine state)])
    (Scooter-start-replace engine)
    (set-current-screen! state 'performing-replacement)
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
  (let ([mode (get-current-screen state)])
    (cond
      [(key-with-ctrl? event #\r)
       (reset-scooter-state! state)
       event-result/consume]
      [(equal? mode 'search-fields) (handle-search-fields-mode-event state event)]
      [(equal? mode 'search-results) (handle-search-results-mode-event state event)]
      [(equal? mode 'performing-replacement) (handle-performing-replacement-event state event)]
      [(equal? mode 'replacement-complete) (handle-replacement-complete-event state event)]
      [else event-result/consume])))

(define (scooter-cursor-handler state _)
  (and (equal? (get-current-screen state) 'search-fields)
       (field-is-text? (get-current-field state))
       (ScooterWindow-cursor-position state)))

(define (start-scooter-search state)
  (clear-all-errors! state)
  (execute-search-process! state))

(define (cancel-search-and-return-to-fields state)
  (let ([engine (get-engine state)])
    (Scooter-cancel-search engine)
    (set-current-screen! state 'search-fields)
    (clear-all-errors! state)))

(define (execute-search-process! state)
  (let* ([engine (get-engine state)]
         [search-term (get-field-text state 'search)]
         [replace-term (get-field-text state 'replace)]
         [fixed-strings (get-field-value state 'fixed-strings)]
         [match-whole-word (get-field-value state 'match-whole-word)]
         [match-case (get-field-value state 'match-case)]
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
          (set-current-screen! state 'search-results)
          (set-lines! state (SearchData 0 #f '() 0))
          (set-completed! state #f)
          (set-selected-index! state 0)
          (set-scroll-offset! state 0)
          (poll-search-results state))
        (handle-search-errors! state response))))

(define (handle-search-errors! state response)
  (let ([error-type (hash-try-get response "error-type")]
        [message (hash-try-get response "message")])
    (cond
      [(equal? error-type "validation-error") (handle-validation-errors! state response)]
      [(equal? error-type "configuration-error")
       (set-general-error! state (or message "Configuration error occurred"))]
      [error-type
       (set-general-error! state
                           (string-append "Error (" error-type "): " (or message "Unknown error")))]
      [else (set-general-error! state "An unexpected error occurred")])))

(define (handle-validation-errors! state response)
  ;; Map validation error response keys to field IDs
  (when (hash-contains? response "search-text-errors")
    (set-field-errors! state 'search (hash-ref response "search-text-errors")))
  (when (hash-contains? response "include-files-errors")
    (set-field-errors! state 'files-include (hash-ref response "include-files-errors")))
  (when (hash-contains? response "exclude-files-errors")
    (set-field-errors! state 'files-exclude (hash-ref response "exclude-files-errors"))))

(define (get-search-status engine)
  (let ([result-count (Scooter-search-result-count engine)]
        [is-complete (Scooter-search-complete? engine)])
    (list is-complete result-count)))

(define (get-search-results engine result-count)
  (if (> result-count 0)
      (Scooter-search-results-window engine 0 (max 0 (- result-count 1)))
      '()))

(define (poll-replacement-progress state)
  (let ([engine (get-engine state)])
    (enqueue-thread-local-callback
     (lambda ()
       (let ([is-complete (Scooter-replacement-complete? engine)])
         (cond
           [is-complete (set-current-screen! state 'replacement-complete)]
           ;; Only continue polling if still on replacement screen
           [(equal? (get-current-screen state) 'performing-replacement)
            (enqueue-thread-local-callback (lambda () (poll-replacement-progress state)))]))))))

(define (poll-search-results state)
  (let ([engine (get-engine state)])
    (enqueue-thread-local-callback
     (lambda ()
       (let ([result-count (Scooter-search-result-count engine)]
             [is-complete (Scooter-search-complete? engine)])

         (set-box! (ScooterWindow-lines-box state)
                   (SearchData result-count is-complete '() (get-scroll-offset state)))
         (fetch-results-window state)

         (cond
           [is-complete (set-box! (ScooterWindow-completed-box state) #t)]
           [else (enqueue-thread-local-callback (lambda () (poll-search-results state)))]))))))
