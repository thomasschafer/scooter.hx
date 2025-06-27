(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/static.scm")

(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx" (only-in Scooter-new))

(require "ui/window.scm")
(require "ui/fields.scm")

(provide scooter)

(define (scooter)
  (define state (create-scooter-window (get-helix-cwd)))

  (push-component!
   (new-component! "scooter-window"
                   state
                   scooter-render
                   (hash "handle_event" scooter-event-handler "cursor" scooter-cursor-handler))))
