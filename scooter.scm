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
         scooter-new)

;; A session is just the opaque Rust engine. Closing a component hides it;
;; quitting or starting fresh explicitly drops this reference after cancelling.
(define *scooter-session* #f)

(define (destroy-session!)
  (when *scooter-session*
    (Scooter-quit *scooter-session*))
  (set! *scooter-session* #f))

(define (resume-session!)
  (let ([window (make-scooter-window *scooter-session*)])
    ;; Results may have arrived while the window was hidden.
    (scooter-response-status (Scooter-pump *scooter-session*))
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
  (set! *scooter-session* (Scooter-engine-new (get-helix-cwd)))
  (resume-session!))

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
