(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-render
                          Scooter-cursor
                          Scooter-handle-key
                          Scooter-paste
                          Scooter-window-size
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

;; Content styles are patched onto whichever fill was painted first.  A source
;; style without a background can safely retain its modifiers while receiving
;; an explicit foreground.  A source background is deliberately discarded by
;; rebuilding from `(style)`: this loses its modifiers, but guarantees that a
;; content run cannot punch through a popup or selected-row fill.
(define (style-with-foreground style-value foreground)
  (if (style->bg style-value)
      (style-fg (style) (colour-or (style->fg style-value) foreground))
      (style-fg style-value (colour-or (style->fg style-value) foreground))))

;; Style invariant (the table is deliberately exhaustive):
;;
;;   tag                             kind
;;   popup, popup-border, toast-border overlay: explicit fg + surface bg
;;   preview                         overlay: explicit fg + editor bg
;;   text, dim, active, error, info    content: fg/modifiers; inherits fill
;;   diff-added, diff-added-emph       content: fg/modifiers; inherits fill
;;   diff-removed, diff-removed-emph   content: fg/modifiers; inherits fill
;;   s:<scope>                         content: fg-patch; inherits fill
;;   selection, selection-secondary    selected-row fill: explicit selection bg
;;   selection-excluded,
;;   selection-secondary-excluded       selected-row fill: explicit error bg
;;
;; The last four are the one intentional kind of content fill: a full result
;; row must replace its background before its text is painted.  Every run
;; layered over it uses one of the content styles above, so it still inherits
;; that selected background.  All popup/toast surfaces use the overlay rule.
;; A content source without a background retains its theme modifiers; a source
;; with a background is rebuilt without modifiers so that background can never
;; punch through the fill beneath it.
(define (style-table)
  (let* ([theme-text (safe-theme-style "ui.text" (style))]
         [theme-background (safe-theme-style "ui.background" (style))]
         [text theme-text]
         [background theme-background]
         ;; Transparent themes leave ui.background (and sometimes ui.text)
         ;; without colours. Color/Reset is the terminal default — the correct
         ;; meaning of "no colour" — and keeps every downstream style-fg /
         ;; style-bg call away from #false, which they reject.
         [foreground
          (colour-or (colour-or (style->fg text) (style->fg theme-text)) Color/Reset)]
         [background-colour
          (colour-or (colour-or (style->bg background) (style->bg theme-background))
                     Color/Reset)]
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
          "dim" (style-with-foreground
                 (safe-theme-style "ui.text.inactive" text)
                 foreground)
          "selection" selection
          "selection-secondary" (style-with-dim selection)
          "selection-excluded" selection-excluded
          "selection-secondary-excluded" (style-with-dim selection-excluded)
          "active" (style-with-foreground
                    (safe-theme-style "ui.text.focus" hint)
                    foreground)
          "popup" popup
          ;; The preview is intentionally editor-native rather than TUI-like:
          ;; it fills only the preview rectangle with ui.background, while its
          ;; content styles remain foreground-only patches over that fill.
          "preview" (style-bg (style-fg text foreground) background-colour)
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

(define (scaled-window-dimension dimension ratio)
  (max 1 (exact (floor (* dimension ratio)))))

(define (centered-window rect ratio)
  (let* ([screen-width (area-width rect)]
         [screen-height (area-height rect)]
         [width (scaled-window-dimension screen-width ratio)]
         [height (scaled-window-dimension screen-height ratio)])
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
(define (scope-tag? tag)
  (and (>= (string-length tag) 2)
       (equal? (substring tag 0 2) "s:")))

;; Scope tags are intentionally resolved per render. A theme can change while
;; Helix is live, and a render-local cache avoids a repeated theme-scope call
;; for every token while retaining that behaviour.
(define (scope-style-for-run styles scope-cache tag)
  (let ([scope (substring tag 2 (string-length tag))]
        [text-style (hash-ref styles "text")])
    (if (hash-contains? (unbox scope-cache) scope)
        (hash-ref (unbox scope-cache) scope)
        (let ([resolved (style-with-foreground
                         (safe-theme-style scope text-style)
                         (style->fg text-style))])
          (set-box! scope-cache (hash-insert (unbox scope-cache) scope resolved))
          resolved))))

(define (style-for-run styles scope-cache tag)
  (if (hash-contains? styles tag)
      (hash-ref styles tag)
      (if (scope-tag? tag)
          (scope-style-for-run styles scope-cache tag)
          (begin
            (unless (member tag (unbox *unknown-scooter-style-tags*))
              (set-box! *unknown-scooter-style-tags*
                        (cons tag (unbox *unknown-scooter-style-tags*)))
              (log::warn! (string-append "scooter-hx: unknown style tag: " tag)))
            (hash-ref styles "text")))))

(define (blit-run! frame content-area styles scope-cache run)
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
                         (style-for-run styles scope-cache tag)))))

(define (scooter-window-render state rect frame)
  (let* ([engine (ScooterWindowState-engine state)]
         [window-area (centered-window rect (Scooter-window-size engine))]
         [content-area (window-content-area window-area)]
         [styles (style-table)]
         [scope-cache (box (hash))]
         [popup-style (hash-ref styles "popup")])
    (buffer/clear frame window-area)
    (block/render frame window-area (make-block popup-style popup-style "all" "plain"))
    (when (> (area-width window-area) 2)
      (frame-set-string! frame
                         (+ (area-x window-area) 2)
                         (area-y window-area)
                         " Scooter "
                         popup-style))
    (for-each (lambda (run) (blit-run! frame content-area styles scope-cache run))
              (Scooter-render engine
                              (area-width content-area)
                              (area-height content-area)))))

(define (scooter-window-cursor state rect)
  (let* ([engine (ScooterWindowState-engine state)]
         [window-area (centered-window rect (Scooter-window-size engine))]
         [content-area (window-content-area window-area)]
         [cursor (Scooter-cursor engine
                                 (area-width content-area)
                                 (area-height content-area))])
    (and cursor
         (position (+ (area-y content-area) (list-ref cursor 1))
                   (+ (area-x content-area) (list-ref cursor 0))))))

;; The engine contract uses Helix's keyboard modifier bits: shift=1,
;; ctrl=2, alt=4, and super=8. Meta=32 is reserved for terminals/frontends
;; that expose it separately, so preserve it too when present.
(define (event-modifiers event)
  (bitwise-and (or (key-event-modifier event) 0) 47))

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
    [(key-event-insert? event) "insert"]
    [(key-event-null? event) "null"]
    [(key-event-caps-lock? event) "capslock"]
    [(key-event-scroll-lock? event) "scrolllock"]
    [(key-event-num-lock? event) "numlock"]
    [(key-event-print-screen? event) "printscreen"]
    [(key-event-pause? event) "pause"]
    [(key-event-menu? event) "menu"]
    [(key-event-keypad-begin? event) "keypadbegin"]
    [(key-event-F? event 1) "f1"]
    [(key-event-F? event 2) "f2"]
    [(key-event-F? event 3) "f3"]
    [(key-event-F? event 4) "f4"]
    [(key-event-F? event 5) "f5"]
    [(key-event-F? event 6) "f6"]
    [(key-event-F? event 7) "f7"]
    [(key-event-F? event 8) "f8"]
    [(key-event-F? event 9) "f9"]
    [(key-event-F? event 10) "f10"]
    [(key-event-F? event 11) "f11"]
    [(key-event-F? event 12) "f12"]
    [(key-event-F? event 13) "f13"]
    [(key-event-F? event 14) "f14"]
    [(key-event-F? event 15) "f15"]
    [(key-event-F? event 16) "f16"]
    [(key-event-F? event 17) "f17"]
    [(key-event-F? event 18) "f18"]
    [(key-event-F? event 19) "f19"]
    [(key-event-F? event 20) "f20"]
    [(key-event-F? event 21) "f21"]
    [(key-event-F? event 22) "f22"]
    [(key-event-F? event 23) "f23"]
    [(key-event-F? event 24) "f24"]
    [else #f]))

;; Pump and key dispatch both return `(status action...)`. Actions mutate
;; Helix only here, leaving Rust independent of the host editor.
(define (open-scooter-file! path line)
  (helix.open path)
  (helix.goto (number->string line))
  (align_view_center))

(define (reload-non-dirty-documents!)
  (for-each editor-document-reload
            (filter (lambda (document) (not (editor-document-dirty? document)))
                    (editor-all-documents))))

;; Opening is queued behind the component event callback. For foreground
;; opens this lets the caller return `hide` and close the popup before Helix
;; changes the active document; background opens retain the component.
(define (enqueue-scooter-open! action)
  (let ([path (list-ref action 1)]
        [line (list-ref action 2)])
    (enqueue-thread-local-callback
     (lambda () (open-scooter-file! path line)))))

(define (consume-scooter-action! action)
  (cond
    [(equal? (car action) "open-file")
     (enqueue-scooter-open! action)
     "hide"]
    [(equal? (car action) "open-file-bg")
     (enqueue-scooter-open! action)
     "rerender"]
    [(equal? (car action) "reload-docs")
     (reload-non-dirty-documents!)
     "rerender"]
    [else
     (log::warn!
      (string-append "scooter-hx: unknown engine action: " (car action)))
     "rerender"]))

(define (consume-scooter-response! response)
  (let ([should-hide #f])
    (for-each (lambda (action)
                (when (equal? (consume-scooter-action! action) "hide")
                  (set! should-hide #t)))
              (cdr response))
    (if should-hide "hide" (car response))))

(define (hide-scooter-window! state)
  (set-box! (ScooterWindowState-visible state) #f)
  (pop-last-component-by-name! "scooter-window"))

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
         (let ([status (consume-scooter-response!
                        (Scooter-pump (ScooterWindowState-engine state)))])
           (when (equal? status "hide")
             (hide-scooter-window! state))
           (when (and (unbox (ScooterWindowState-visible state))
                      (Scooter-busy? (ScooterWindowState-engine state)))
             (start-scooter-poll-loop! state))))))))

;; Returns the response status string so the entry point can own session teardown.
(define (scooter-window-event-handler state event)
  (let ([response (if (paste-event? event)
                      (Scooter-paste (ScooterWindowState-engine state)
                                     (or (paste-event-string event) ""))
                      (let ([code (event-code event)])
                        (and code
                             (Scooter-handle-key (ScooterWindowState-engine state)
                                                 code
                                                 (event-modifiers event)))))])
    (if response
        (let ([status (consume-scooter-response! response)])
          (when (Scooter-busy? (ScooterWindowState-engine state))
            (start-scooter-poll-loop! state))
          status)
        "rerender")))
