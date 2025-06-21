(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))
(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")

(require "ui/scooter-window.scm")
(require "ui/state-init.scm")

(provide scooter)

(define (scooter)
  (define state
    (ScooterWindow 'input ; Start in input mode
                   (create-initial-field-values) ; Field values from registry
                   (create-initial-cursor-positions) ; Cursor positions for text fields
                   'search ; Start with search field active
                   (box '()) ; Empty results
                   #f ; No process yet
                   #f ; No stdout port yet
                   (box #f) ; Process not completed
                   (position 0 0) ; Initial cursor position
                   '())) ; Empty debug events

  (push-component!
   (new-component! "scooter-window"
                   state
                   scooter-render
                   (hash "handle_event" scooter-event-handler "cursor" scooter-cursor-handler))))
