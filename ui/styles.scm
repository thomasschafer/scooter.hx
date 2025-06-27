(require-builtin helix/components)

(provide UIStyles
         UIStyles-text
         UIStyles-popup
         UIStyles-active
         UIStyles-dim
         UIStyles-search
         UIStyles-status
         UIStyles-line-num
         ui-styles)

(struct UIStyles (text popup active dim search status line-num))

(define (ui-styles)
  (UIStyles (theme-scope *helix.cx* "ui.text") ; text
            (theme-scope *helix.cx* "ui.popup") ; popup
            (style-with-bold (theme-scope *helix.cx* "hint")) ; active
            (theme-scope *helix.cx* "ui.text.inactive") ; dim
            (theme-scope *helix.cx* "search.match") ; search
            (theme-scope *helix.cx* "ui.statusline") ; status
            (theme-scope *helix.cx* "special") ; line-num
            ))
