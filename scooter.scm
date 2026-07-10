(require "helix/components.scm")
(require "helix/misc.scm")
(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-engine-new
                          Scooter-render
                          Scooter-handle-key))

(require "ui/spike.scm")

(provide scooter)

;;@doc
;; Open the Scooter rewrite toolchain spike.
(define (scooter)
  (let ([engine (Scooter-engine-new)])
    (push-component!
     (new-component! "scooter-s1"
                     engine
                     scooter-render
                     (hash "handle_event" scooter-event-handler)))))
