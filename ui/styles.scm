(require-builtin helix/components)

(provide UIStyles
         UIStyles-text
         UIStyles-popup
         UIStyles-active
         UIStyles-dim
         UIStyles-status
         UIStyles-line-num
         UIStyles-selection
         UIStyles-cursor
         UIStyles-error
         UIStyles-warning
         UIStyles-info
         UIStyles-fg
         UIStyles-bg
         ui-styles)

(struct UIStyles (text popup active dim status line-num selection cursor error warning info fg bg))

(define (ui-styles)
  (UIStyles (theme-scope *helix.cx* "ui.text") ; text
            (theme-scope *helix.cx* "ui.popup") ; popup
            (style-underline-style (style-with-bold (theme-scope *helix.cx* "hint")) Underline/Reset) ; active
            (theme-scope *helix.cx* "ui.text.inactive") ; dim
            (theme-scope *helix.cx* "ui.statusline") ; status
            (theme-scope *helix.cx* "special") ; line-num
            (theme-scope *helix.cx* "ui.selection") ; selection
            (theme-scope *helix.cx* "ui.cursor") ; cursor
            (theme-scope *helix.cx* "error") ; error
            (theme-scope *helix.cx* "warning") ; warning
            (theme-scope *helix.cx* "info") ; info
            (theme->fg *helix.cx*) ; fg
            (theme->bg *helix.cx*) ; bg
            ))
