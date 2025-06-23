(provide calculate-centered-layout
         CenteredLayout
         CenteredLayout-x
         CenteredLayout-y
         CenteredLayout-width
         CenteredLayout-height)

(struct CenteredLayout
        (x ; X position after centering
         y ; Y position after centering
         width ; Actual content width (may be constrained)
         height ; Actual content height
         ))

(define (calculate-centered-layout container-x
                                   container-y
                                   container-width
                                   container-height
                                   content-width
                                   content-height
                                   max-width
                                   max-height
                                   center-horizontal?
                                   center-vertical?)
  (let* ([actual-width (if (and max-width (> content-width max-width)) max-width content-width)]
         [actual-height (if (and max-height (> content-height max-height)) max-height content-height)]
         [h-offset (if center-horizontal?
                       (quotient (- container-width actual-width) 2)
                       0)]
         [v-offset (if center-vertical?
                       (quotient (- container-height actual-height) 2)
                       0)]
         [final-x (+ container-x h-offset)]
         [final-y (+ container-y v-offset)])
    (CenteredLayout final-x final-y actual-width actual-height)))
