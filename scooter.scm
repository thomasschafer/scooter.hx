(require "helix/components.scm")
(require "helix/misc.scm")

(require-builtin steel/ffi)

(require "ui/window.scm")
(require "ui/fields.scm")

(provide scooter
         scooter-new)

(define *scooter-session* #f)

(define (destroy-session)
  (when *scooter-session*
    (cancel-all-operations! *scooter-session*))
  (set! *scooter-session* #f))

(define (create-new-session)
  (set! *scooter-session* (create-scooter-window))
  (resume-session))

(define (resume-session)
  (push-component!
   (new-component!
    "scooter-window"
    *scooter-session*
    scooter-render
    (hash "handle_event" (make-scooter-event-handler) "cursor" scooter-cursor-handler))))

(define (make-scooter-event-handler)
  (lambda (state event)
    (cond
      [(key-event-escape? event) event-result/close]
      [(and (key-event-char event)
            (equal? (key-event-char event) #\c)
            (equal? (key-event-modifier event) key-modifier-ctrl))
       (enqueue-thread-local-callback (lambda () (destroy-session)))
       event-result/close]
      [else
       (let ([result (scooter-event-handler state event)])
         (if (equal? result 'destroy-and-close)
             (begin
               (enqueue-thread-local-callback (lambda () (destroy-session)))
               event-result/close)
             result))])))

(define (scooter)
  (if *scooter-session*
      (resume-session)
      (create-new-session)))

(define (scooter-new)
  (destroy-session)
  (create-new-session))
