(define package-name 'scooter)
(define version "0.1.1")

(define dependencies '())

(define (asset-url asset-name)
  (string-append "https://github.com/thomasschafer/scooter.hx/releases/download/v"
                 version
                 "/"
                 asset-name))

(define dylibs
  `((#:name "scooter_hx"
     #:urls ((#:platform "x86_64-linux" #:url ,(asset-url "libscooter_hx.x86_64-linux.so"))
             (#:platform "aarch64-linux" #:url ,(asset-url "libscooter_hx.aarch64-linux.so"))
             (#:platform "x86_64-macos" #:url ,(asset-url "libscooter_hx.x86_64-macos.dylib"))
             (#:platform "aarch64-macos" #:url ,(asset-url "libscooter_hx.aarch64-macos.dylib"))
             (#:platform "x86_64-windows" #:url ,(asset-url "libscooter_hx.x86_64-windows.dll"))))))
