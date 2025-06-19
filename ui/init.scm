;; Main entry point for Scooter search plugin
(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/configuration.scm")
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "components/scooter-window.scm")

(provide scooter)

;; Main plugin function
(define (scooter)
  ;; Create initial state in input mode
  (define state
    (ScooterWindow 'input ; mode
                   "" ; search-term (empty)
                   "" ; replace-term (empty)
                   'search ; current-field (start with search)
                   (box '()) ; lines-box
                   #f ; process (none yet)
                   #f ; stdout-port (none yet)
                   (box #f) ; completed-box
                   (position 0 0))) ; cursor-position

  (push-component!
   (new-component! "scooter-window"
                   state
                   scooter-render
                   (hash "handle_event" scooter-event-handler "cursor" scooter-cursor-handler))))
