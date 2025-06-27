(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/static.scm")

(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx" (only-in Scooter-new))

(require "ui/window.scm")
(require "ui/fields.scm")

(provide scooter)

(define (scooter)
  (define state
    (ScooterWindow (box 'search-fields)
                   (box (create-initial-field-values))
                   (box (create-initial-cursor-positions))
                   (box 'search)
                   (box '())
                   (box #f)
                   (box #f)
                   (box #f)
                   (position 0 0)
                   (box '())
                   (box (Scooter-new (get-helix-cwd)))))

  (push-component!
   (new-component! "scooter-window"
                   state
                   scooter-render
                   (hash "handle_event" scooter-event-handler "cursor" scooter-cursor-handler))))
