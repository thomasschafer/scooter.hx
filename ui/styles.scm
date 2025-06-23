(require-builtin helix/components)

(provide UIStyles
         UIStyles-text
         UIStyles-popup
         UIStyles-active
         UIStyles-dim
         UIStyles-search
         UIStyles-status
         create-ui-styles)

(struct UIStyles (text popup active dim search status))

(define (create-ui-styles)
  (let ([ui-text-style (theme-scope *helix.cx* "ui.text")])
    (UIStyles ui-text-style ; text
              (theme-scope *helix.cx* "ui.popup") ; popup
              (style-with-bold (theme-scope *helix.cx* "ui.text.focus")) ; active
              (style-with-dim ui-text-style) ; dim
              (theme-scope *helix.cx* "search.match") ; search
              (theme-scope *helix.cx* "ui.statusline") ; status
              )))
