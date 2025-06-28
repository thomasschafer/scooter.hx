(require-builtin helix/components)

(provide UIStyles
         UIStyles-text
         UIStyles-popup
         UIStyles-active
         UIStyles-dim
         UIStyles-status
         UIStyles-line-num
         ui-styles
         get-theme-colors)

(struct UIStyles (text popup active dim status line-num))

(define (ui-styles)
  (UIStyles (theme-scope *helix.cx* "ui.text") ; text
            (theme-scope *helix.cx* "ui.popup") ; popup
            (style-with-bold (theme-scope *helix.cx* "hint")) ; active
            (theme-scope *helix.cx* "ui.text.inactive") ; dim
            (theme-scope *helix.cx* "ui.statusline") ; status
            (theme-scope *helix.cx* "special") ; line-num
            ))

(define (get-theme-colors)
  (hash "fg"
        (theme->fg *helix.cx*)
        "bg"
        (theme->bg *helix.cx*)
        "selection"
        (theme-scope *helix.cx* "ui.selection")
        "cursor"
        (theme-scope *helix.cx* "ui.cursor")
        "error"
        (theme-scope *helix.cx* "error")
        "warning"
        (theme-scope *helix.cx* "warning")
        "info"
        (theme-scope *helix.cx* "info")))
