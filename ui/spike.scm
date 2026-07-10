(require "helix/components.scm")
(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-render
                          Scooter-handle-key))

(provide scooter-render
         scooter-event-handler)

;; Helix may not define every semantic scope in a theme. Keep rendering alive
;; when that happens, using the supplied fallback style instead.
(define (safe-theme-scope scope fallback)
  (with-handler (lambda (_) fallback)
    (theme-scope scope)))

(define (style-table)
  (let* ([text (safe-theme-scope "ui.text" (style))]
         [hint (safe-theme-scope "hint" text)])
    (hash "text" text
          "dim" (safe-theme-scope "ui.text.inactive" text)
          "selection" (safe-theme-scope "ui.selection" text)
          "active" (safe-theme-scope "ui.text.focus" hint)
          "error" (safe-theme-scope "error" text)
          "info" (safe-theme-scope "info" text)
          "diff-added" (safe-theme-scope "diff.plus" text)
          "diff-removed" (safe-theme-scope "diff.minus" text))))

(define (centered-window rect)
  (let* ([screen-width (area-width rect)]
         [screen-height (area-height rect)]
         [width (max 1 (quotient (* screen-width 9) 10))]
         [height (max 1 (quotient (* screen-height 9) 10))])
    (area (+ (area-x rect) (quotient (- screen-width width) 2))
          (+ (area-y rect) (quotient (- screen-height height) 2))
          width
          height)))

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

(define (scooter-render engine rect frame)
  (let* ([window-area (centered-window rect)]
         [window-width (area-width window-area)]
         [window-height (area-height window-area)]
         [content-area (area (+ (area-x window-area) 1)
                             (+ (area-y window-area) 1)
                             (max 0 (- window-width 2))
                             (max 0 (- window-height 2)))]
         ;; Construct this exactly once for this frame, rather than once per run.
         [styles (style-table)]
         [popup-style (safe-theme-scope "ui.popup" (hash-ref styles "text"))])
    (buffer/clear frame window-area)
    (block/render frame window-area (make-block popup-style popup-style "all" "plain"))
    (for-each (lambda (run) (blit-run! frame content-area styles run))
              (Scooter-render engine
                              (area-width content-area)
                              (area-height content-area)))))

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

(define (scooter-event-handler engine event)
  (let ([code (event-code event)])
    (if code
        (let ([status (Scooter-handle-key engine code (event-modifiers event))])
          (if (or (equal? status "hide") (equal? status "quit"))
              event-result/close
              event-result/consume))
        event-result/consume)))
