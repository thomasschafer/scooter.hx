(require "helix/ext.scm")
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))

(require-builtin helix/core/text as text::)
(require-builtin helix/core/static as static::)
(require-builtin helix/core/misc as misc::)

(require "ui/window.scm")
(require "ui/state-init.scm")

(provide scooter)

(define (scooter)
  (define state
    (ScooterWindow (box 'input) ; mode-box
                   (box (create-initial-field-values)) ; field-values-box from registry
                   (box (create-initial-cursor-positions)) ; cursor-positions-box for text fields
                   (box 'search) ; current-field-box
                   (box '()) ; lines-box - Empty results
                   (box #f) ; process-box - No process yet
                   (box #f) ; stdout-port-box - No stdout port yet
                   (box #f) ; completed-box - Process not completed
                   (position 0 0) ; cursor-position - Initial cursor position (immutable)
                   (box '()))) ; debug-events-box - Empty debug events

  (push-component!
   (new-component! "scooter-window"
                   state
                   scooter-render
                   (hash "handle_event" scooter-event-handler "cursor" scooter-cursor-handler))))
