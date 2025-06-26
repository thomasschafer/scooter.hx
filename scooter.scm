(require "helix/ext.scm")
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require (prefix-in helix. "helix/commands.scm"))

(require-builtin helix/core/text as text::)
(require-builtin helix/core/static as static::)
(require-builtin helix/core/misc as misc::)

(require-builtin steel/ffi)

(#%require-dylib "libscooter_hx"
                 (only-in Scooter-new
                          Scooter-reset
                          Scooter-start-search
                          Scooter-cancel-search
                          Scooter-search-complete?
                          Scooter-search-result-count
                          Scooter-search-results-window
                          Scooter-toggle-inclusion
                          Scooter-start-replace
                          Scooter-num-replacements-complete
                          Scooter-replacement-complete?
                          Scooter-cancel-replacement
                          Scooter-replacement-stats))

(require "ui/window.scm")
(require "ui/state-init.scm")

(provide scooter)

(define (scooter)
  (define state
    (ScooterWindow (box 'input)
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
