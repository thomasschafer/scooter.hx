(require "helix/components.scm")
(require "helix/misc.scm")
(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-render
                          Scooter-cursor
                          Scooter-handle-key
                          Scooter-pump
                          Scooter-busy?))

(provide ScooterWindowState
         ScooterWindowState-engine
         ScooterWindowState-visible
         ScooterWindowState-polling
         make-scooter-window
         scooter-window-render
         scooter-window-cursor
         scooter-window-event-handler
         consume-scooter-response!
         start-scooter-poll-loop!)

(struct ScooterWindowState (engine visible polling))

(define (make-scooter-window engine)
  (ScooterWindowState engine (box #t) (box #f)))

;; `theme->fg` and `theme->bg` in this Helix build each return a Style (not a
;; colour) and are deprecated in favour of `theme-scope`.  Guard both a
;; missing scope and an unexpected result here, so every style accessor below
;; receives a Style and every colour fallback stays a Color-or-#false.
(define (safe-theme-style scope fallback)
  (with-handler (lambda (_) fallback)
    (let ([resolved (theme-scope scope)])
      (if (Style? resolved) resolved fallback))))

(define (colour-or colour fallback)
  (if colour colour fallback))

;; `frame-set-string!` patches cells instead of replacing their complete style.
;; Overlay runs therefore need both colours set even when a theme scope leaves
;; one of them unspecified; otherwise they inherit a colour from the content
;; beneath them. Keep the scope's own colours where present and complete the
;; rest from the theme's base foreground/background.
(define (style-with-explicit-colours style foreground background)
  (style-bg
   (style-fg style (colour-or (style->fg style) foreground))
   (colour-or (style->bg style) background)))

;; Content styles are patched onto whichever fill was painted first.  Start
;; from a new Style so a theme scope's optional background cannot punch a hole
;; through a popup or a selected result row.
(define (style-with-foreground style-value foreground)
  (style-fg (style) (colour-or (style->fg style-value) foreground)))

;; Style invariant (the table is deliberately exhaustive):
;;
;;   tag                             kind
;;   popup, popup-border, toast-border overlay: explicit fg + surface bg
;;   text, active, error, info         content: fg/modifiers; inherits fill
;;   diff-added, diff-added-emph       content: fg/modifiers; inherits fill
;;   diff-removed, diff-removed-emph   content: fg/modifiers; inherits fill
;;   selection, selection-secondary    selected-row fill: explicit selection bg
;;   selection-excluded,
;;   selection-secondary-excluded       selected-row fill: explicit error bg
;;
;; The last four are the one intentional kind of content fill: a full result
;; row must replace its background before its text is painted.  Every run
;; layered over it uses one of the content styles above, so it still inherits
;; that selected background.  All popup/toast surfaces use the overlay rule.
(define (style-table)
  (let* ([theme-text (safe-theme-style "ui.text" (style))]
         [theme-background (safe-theme-style "ui.background" (style))]
         [text theme-text]
         [background theme-background]
         [foreground (colour-or (style->fg text) (style->fg theme-text))]
         [background-colour
          (colour-or (style->bg background) (style->bg theme-background))]
         [hint (style-with-foreground (safe-theme-style "hint" text) foreground)]
         [selection (style-with-explicit-colours
                     (safe-theme-style "ui.selection" text)
                     foreground
                     background-colour)]
         [error (style-with-foreground (safe-theme-style "error" text) foreground)]
         [error-background (colour-or (style->fg error) background-colour)]
         [selection-excluded
          (style-bg (style-fg (style) foreground) error-background)]
         [popup-scope (safe-theme-style "ui.popup" text)]
         ;; A popup's own background wins when it is present.  Every overlay
         ;; cell below uses this exact resolved surface so `frame-set-string!`
         ;; can never inherit a different background from content underneath.
         [surface-bg (colour-or (style->bg popup-scope) background-colour)]
         [popup (style-with-explicit-colours popup-scope foreground surface-bg)]
         [diff-added (style-with-foreground
                      (safe-theme-style "diff.plus" text)
                      foreground)])
    (hash "text" (style-with-foreground text foreground)
          "selection" selection
          "selection-secondary" (style-with-dim selection)
          "selection-excluded" selection-excluded
          "selection-secondary-excluded" (style-with-dim selection-excluded)
          "active" (style-with-foreground
                    (safe-theme-style "ui.text.focus" hint)
                    foreground)
          "popup" popup
          "popup-border" (style-with-explicit-colours
                           diff-added
                           foreground
                           surface-bg)
          "toast-border" (style-with-explicit-colours
                           diff-added
                           foreground
                           surface-bg)
          "error" error
          "info" (style-with-foreground (safe-theme-style "info" text) foreground)
          "diff-added" diff-added
          "diff-added-emph" (style-with-reversed diff-added)
          "diff-removed" (style-with-foreground
                           (safe-theme-style "diff.minus" text)
                           foreground)
          "diff-removed-emph" (style-with-reversed
                                (style-with-foreground
                                 (safe-theme-style "diff.minus" text)
                                 foreground)))))

(define (centered-window rect)
  (let* ([screen-width (area-width rect)]
         [screen-height (area-height rect)]
         [width (max 1 (quotient (* screen-width 9) 10))]
         [height (max 1 (quotient (* screen-height 9) 10))])
    (area (+ (area-x rect) (quotient (- screen-width width) 2))
          (+ (area-y rect) (quotient (- screen-height height) 2))
          width
          height)))

(define (window-content-area window-area)
  (area (+ (area-x window-area) 1)
        (+ (area-y window-area) 1)
        (max 0 (- (area-width window-area) 2))
        (max 0 (- (area-height window-area) 2))))

(define *unknown-scooter-style-tags* (box '()))

;; Rust's tag enum makes this path exceptional, but do not allow a future
;; engine/style-table mismatch to take down Helix's render callback.  Warn only
;; once for each tag; a stale dylib can otherwise redraw thousands of frames.
(define (style-for-run styles tag)
  (if (hash-contains? styles tag)
      (hash-ref styles tag)
      (begin
        (unless (member tag (unbox *unknown-scooter-style-tags*))
          (set-box! *unknown-scooter-style-tags*
                    (cons tag (unbox *unknown-scooter-style-tags*)))
          (log::warn! (string-append "scooter-hx: unknown style tag: " tag)))
        (hash-ref styles "text"))))

(define (blit-run! frame content-area styles run)
  (let ([x (list-ref run 0)]
        [y (list-ref run 1)]
        [text (list-ref run 2)]
        [tag (list-ref run 3)])
    (when (and (< x (area-width content-area))
               (< y (area-height content-area)))
      (frame-set-string! frame
                         (+ (area-x content-area) x)
                         (+ (area-y content-area) y)
                         text
                         (style-for-run styles tag)))))

(define (scooter-window-render state rect frame)
  (let* ([window-area (centered-window rect)]
         [content-area (window-content-area window-area)]
         [styles (style-table)]
         [popup-style (hash-ref styles "popup")]
         [engine (ScooterWindowState-engine state)])
    (buffer/clear frame window-area)
    (block/render frame window-area (make-block popup-style popup-style "all" "plain"))
    (when (> (area-width window-area) 2)
      (frame-set-string! frame
                         (+ (area-x window-area) 2)
                         (area-y window-area)
                         " Scooter "
                         popup-style))
    (for-each (lambda (run) (blit-run! frame content-area styles run))
              (Scooter-render engine
                              (area-width content-area)
                              (area-height content-area)))))

(define (scooter-window-cursor state rect)
  (let* ([window-area (centered-window rect)]
         [content-area (window-content-area window-area)]
         [cursor (Scooter-cursor (ScooterWindowState-engine state)
                                 (area-width content-area)
                                 (area-height content-area))])
    (and cursor
         (position (+ (area-y content-area) (list-ref cursor 1))
                   (+ (area-x content-area) (list-ref cursor 0))))))

;; The engine contract deliberately contains only portable key names and the
;; shift/ctrl/alt bits. Helix's super bit is not part of that contract.
(define (event-modifiers event)
  (bitwise-and (or (key-event-modifier event) 0) 7))

(define (event-code event)
  (cond
    [(key-event-char event) (make-string 1 (key-event-char event))]
    [(key-event-escape? event) "esc"]
    [(key-event-enter? event) "enter"]
    [(key-event-tab? event) "tab"]
    [(key-event-backspace? event) "backspace"]
    [(key-event-left? event) "left"]
    [(key-event-right? event) "right"]
    [(key-event-up? event) "up"]
    [(key-event-down? event) "down"]
    [(key-event-home? event) "home"]
    [(key-event-end? event) "end"]
    [(key-event-page-up? event) "pageup"]
    [(key-event-page-down? event) "pagedown"]
    [(key-event-delete? event) "delete"]
    [else #f]))

;; Pump and key dispatch both return `(status action...)`. H3 will turn
;; `open-file` into a Helix editor action; until then keep the queue visible in
;; the Helix log instead of silently dropping it at the FFI boundary.
(define (consume-scooter-action! action)
  (when (equal? (car action) "open-file")
    (log::info!
     (string-append "scooter-hx: ignoring open-file until H3: "
                    (list-ref action 1)
                    ":"
                    (number->string (list-ref action 2))))))

(define (consume-scooter-response! response)
  (for-each consume-scooter-action! (cdr response))
  (car response))

;; Polling is owned by the component state: a closed component marks itself
;; invisible, so a delayed callback can never pump a hidden stale window.
(define (start-scooter-poll-loop! state)
  (when (and (unbox (ScooterWindowState-visible state))
             (not (unbox (ScooterWindowState-polling state))))
    (set-box! (ScooterWindowState-polling state) #t)
    (enqueue-thread-local-callback-with-delay
     50
     (lambda ()
       (set-box! (ScooterWindowState-polling state) #f)
       (when (unbox (ScooterWindowState-visible state))
         (consume-scooter-response!
          (Scooter-pump (ScooterWindowState-engine state)))
         (when (Scooter-busy? (ScooterWindowState-engine state))
           (start-scooter-poll-loop! state)))))))

;; Returns the response status string so the entry point can own session teardown.
(define (scooter-window-event-handler state event)
  (let ([code (event-code event)])
    (if code
        (let ([status (consume-scooter-response!
                       (Scooter-handle-key (ScooterWindowState-engine state)
                                           code
                                           (event-modifiers event)))])
          (when (Scooter-busy? (ScooterWindowState-engine state))
            (start-scooter-poll-loop! state))
          status)
        "rerender")))
