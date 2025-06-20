;; Main entry point for Scooter search plugin
(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/configuration.scm")
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "components/scooter-window.scm")
(require "components/field-registry.scm")
(require (only-in "components/field-registry.scm" FIELD-TYPE-TEXT FIELD-TYPE-BOOLEAN))

(provide scooter)

;; Main plugin function
(define (scooter)
  ;; Initialize field values from registry
  (define field-values
    (fold (lambda (field-def values)
            (hash-insert values (field-id field-def) (field-default-value field-def)))
          (hash)
          (get-all-fields)))

  ;; Initialize cursor positions for text fields
  (define cursor-positions
    (fold (lambda (field-def positions)
            (if (equal? (field-type field-def) FIELD-TYPE-TEXT)
                (hash-insert positions (field-id field-def) 0)
                positions))
          (hash)
          (get-all-fields)))

  ;; Create initial state in input mode
  (define state
    (ScooterWindow 'input ; mode
                   field-values ; field-values hash
                   cursor-positions ; cursor-positions hash
                   'search ; current-field (start with search)
                   (box '()) ; lines-box
                   #f ; process (none yet)
                   #f ; stdout-port (none yet)
                   (box #f) ; completed-box
                   (position 0 0) ; cursor-position
                   '())) ; debug-events (empty list)

  (push-component!
   (new-component! "scooter-window"
                   state
                   scooter-render
                   (hash "handle_event" scooter-event-handler "cursor" scooter-cursor-handler))))
