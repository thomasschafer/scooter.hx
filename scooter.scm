(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-engine-new
                          Scooter-pump
                          Scooter-busy?
                          Scooter-quit))

(require "ui/window.scm")

(provide scooter
         scooter-new
         scooter-set!
         scooter-keys!)

;; A session is just the opaque Rust engine. Closing a component hides it;
;; quitting or starting fresh explicitly drops this reference after cancelling.
(define *scooter-session* #f)

;; These settings are intentionally retained in Steel rather than read from
;; scooter's config.toml. They are consumed only when Rust creates an engine,
;; so a live hidden session keeps its current behaviour until `scooter-new`.
(define *scooter-options* (box '()))

(define (remember-scooter-option! key value)
  ;; Preserve call order: later calls override an earlier setting for the same
  ;; path when the Rust parser applies this list.
  (set-box! *scooter-options*
            (append (unbox *scooter-options*) (list (list key value)))))

(define (scooter-setting-path setting)
  (cond
    [(eq? setting 'multiline) "search.multiline"]
    [(eq? setting 'hidden) "search.hidden"]
    [(eq? setting 'advanced-regex) "search.advanced-regex"]
    [(eq? setting 'include-git-folders) "search.include-git-folders"]
    [(eq? setting 'escape-sequences) "search.escape-sequences"]
    [(eq? setting 'wrap-text) "preview.wrap-text"]
    [(eq? setting 'window-size) "window.size"]
    [(eq? setting 'runtime-dir) "highlight.runtime-dir"]
    [else (error (string-append "Unknown Scooter setting: " (to-string setting)))]))

;;@doc
;; Set a Scooter behaviour option. Accepted symbols are `multiline`, `hidden`,
;; `advanced-regex`, `include-git-folders`, `escape-sequences`, `wrap-text`,
;; `window-size`, and `runtime-dir`. Settings affect newly created sessions only: the next
;; `:scooter-new`, or the first `:scooter` when no session is active.
(define (scooter-set! setting value)
  (remember-scooter-option! (scooter-setting-path setting) value))

;;@doc
;; Replace a Scooter binding, using core's usual key syntax such as `"C-o"`,
;; `"A-m"`, or `"S-tab"`. The binding path excludes the `keys.` prefix, for
;; example `(scooter-keys! "search.results.move_down" '("j" "down"))`.
;; A single string is accepted in place of the list. Settings affect newly
;; created sessions only: the next `:scooter-new`, or the first `:scooter`.
(define (scooter-keys! binding value)
  (remember-scooter-option!
   (string-append "keys." binding)
   (if (string? value) (list value) value)))

(define (destroy-session!)
  (when *scooter-session*
    (Scooter-quit *scooter-session*))
  (set! *scooter-session* #f))

(define (resume-session!)
  (let ([window (make-scooter-window *scooter-session*)])
    ;; Results may have arrived while the window was hidden.
    (consume-scooter-response! (Scooter-pump *scooter-session*))
    (push-component!
     (new-component!
      "scooter-window"
      window
      scooter-window-render
      (hash "handle_event"
            (lambda (state event)
              (let ([status (scooter-window-event-handler state event)])
                (cond
                  [(equal? status "hide")
                   (set-box! (ScooterWindowState-visible state) #f)
                   event-result/close]
                  [(equal? status "quit")
                   (set-box! (ScooterWindowState-visible state) #f)
                   (destroy-session!)
                   event-result/close]
                  [else event-result/consume])))
            "cursor" scooter-window-cursor)))
    (start-scooter-poll-loop! window)))

(define (create-session!)
  (let ([engine (Scooter-engine-new (get-helix-cwd) (unbox *scooter-options*))])
    (if (string? engine)
        (set-error! engine)
        (begin
          (set! *scooter-session* engine)
          (resume-session!)))))

;;@doc
;; Resume an existing Scooter session, or create one for Helix's current directory.
(define (scooter)
  (if *scooter-session*
      (resume-session!)
      (create-session!)))

;;@doc
;; Cancel the current Scooter session and open a fresh one.
(define (scooter-new)
  (destroy-session!)
  (create-session!))
