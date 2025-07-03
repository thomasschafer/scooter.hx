(define package-name 'scooter)
(define version "0.1.0")

(define dependencies '())
(define dylibs
  '((#:name "scooter_hx"
     #:urls
     ((#:platform "x86_64-windows"
       #:url
       "https://github.com/thomasschafer/scooter.hx/releases/download/v0.1.0/libscooter_hx.x86_64-windows.dll")
      (#:platform "x86_64-macos"
       #:url
       "https://github.com/thomasschafer/scooter.hx/releases/download/v0.1.0/libscooter_hx.aarch64-macos.dylib")
      (#:platform "aarch_64-macos"
       #:url
       "https://github.com/thomasschafer/scooter.hx/releases/download/v0.1.0/libscooter_hx.aarch64-macos.dylib")
      (#:platform "x86_64-linux"
       #:url
       "https://github.com/thomasschafer/scooter.hx/releases/download/v0.1.0/libscooter_hx.x86_64-linux.so")
      (#:platform "aarch64-linux"
       #:url
       "https://github.com/thomasschafer/scooter.hx/releases/download/v0.1.0/libscooter_hx.aarch64-linux.so")))))
