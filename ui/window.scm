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
         start-scooter-poll-loop!)

(struct ScooterWindowState (engine visible polling))

(define (make-scooter-window engine)
  (ScooterWindowState engine (box #t) (box #f)))

;; Helix may not define every semantic scope in a theme. Keep rendering alive
;; when that happens, using the supplied fallback style instead.
(define (safe-theme-scope scope fallback)
  (with-handler (lambda (_) fallback)
    (theme-scope scope)))

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

(define (style-table)
  (let* ([text (safe-theme-scope "ui.text" (style))]
         [background (safe-theme-scope "ui.background" (theme->bg *helix.cx*))]
         [foreground (colour-or (style->fg (theme->fg *helix.cx*)) (style->fg text))]
         [background-colour
          (colour-or (style->bg (theme->bg *helix.cx*)) (style->bg background))]
         [hint (safe-theme-scope "hint" text)]
         [selection (style-with-explicit-colours
                     (safe-theme-scope "ui.selection" text)
                     foreground
                     background-colour)]
         [error (safe-theme-scope "error" text)]
         [error-background (colour-or (style->fg error) background-colour)]
         [selection-excluded
          (style-bg (style-fg (style) foreground) error-background)]
         [popup (style-with-explicit-colours
                 (safe-theme-scope "ui.popup" text)
                 foreground
                 background-colour)]
         [diff-added (safe-theme-scope "diff.plus" text)])
    (hash "text" text
          "dim" (safe-theme-scope "ui.text.inactive" text)
          "selection" selection
          "selection-secondary" (style-with-dim selection)
          "selection-excluded" selection-excluded
          "selection-secondary-excluded" (style-with-dim selection-excluded)
          "active" (safe-theme-scope "ui.text.focus" hint)
          "popup" popup
          "toast-border" (style-with-explicit-colours
                           diff-added
                           foreground
                           background-colour)
          "error" error
          "info" (safe-theme-scope "info" text)
          "diff-added" diff-added
          "diff-added-emph" (style-with-reversed (safe-theme-scope "diff.plus" text))
          "diff-removed" (safe-theme-scope "diff.minus" text)
          "diff-removed-emph" (style-with-reversed (safe-theme-scope "diff.minus" text)))))

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
                         (hash-ref styles tag)))))

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
         (Scooter-pump (ScooterWindowState-engine state))
         (when (Scooter-busy? (ScooterWindowState-engine state))
           (start-scooter-poll-loop! state)))))))

;; Returns the engine status string so the entry point can own session teardown.
(define (scooter-window-event-handler state event)
  (let ([code (event-code event)])
    (if code
        (let ([status (Scooter-handle-key (ScooterWindowState-engine state)
                                          code
                                          (event-modifiers event))])
          (when (Scooter-busy? (ScooterWindowState-engine state))
            (start-scooter-poll-loop! state))
          status)
        "rerender")))
