;; Command builder for scooter CLI
;; Constructs scooter command arguments from field values
(require "field-registry.scm")

(provide build-scooter-args)

;; Build scooter command arguments from field values
(define (build-scooter-args field-values)
  (define search-term (hash-ref field-values 'search))
  (define replace-term (hash-ref field-values 'replace))

  ;; Start with base arguments
  (define base-args (list "-N" "-s" search-term "-r" replace-term))

  ;; Add boolean flags
  (define boolean-flags
    (filter (lambda (arg) arg)
            (list (and (hash-ref field-values 'fixed-strings) "-f")
                  (and (hash-ref field-values 'match-whole-word) "-w")
                  (and (not (hash-ref field-values 'match-case))
                       "-i")))) ; case-insensitive when NOT match-case

  ;; Add file pattern arguments
  (define pattern-args
    (append (let ([include-pattern (hash-ref field-values 'files-include)])
              (if (> (string-length include-pattern) 0)
                  (list "-I" include-pattern)
                  '()))
            (let ([exclude-pattern (hash-ref field-values 'files-exclude)])
              (if (> (string-length exclude-pattern) 0)
                  (list "-E" exclude-pattern)
                  '()))))

  ;; Combine all arguments
  (append base-args boolean-flags pattern-args))
