(define package-name 'scooter)
(define version "0.1.0")

(define dependencies '())

; TODO: add github action to check if version is up to date
(define dylibs
  '((#:name "scooter_hx"
     #:urls
     ((#:platform "x86_64-windows"
       #:url
       "https://github.com/thomasschafer/scooter.git/releases/download/v0.1.0/libscooter_hx.dll")
      (#:platform "x86_64-macos"
       #:url
       "https://github.com/thomasschafer/scooter.git/releases/download/v0.1.0/libscooter_hx.dylib")
      (#:platform "aarch64-macos"
       #:url
       "https://github.com/thomasschafer/scooter.git/releases/download/v0.1.0/libscooter_hx.dylib")
      (#:platform "x86_64-macos"
       #:url "https://github.com/thomasschafer/scooter.git/releases/download/v0.1.0/libscooter_hx.so")
      (#:platform "aarch64-linux"
       #:url
       "https://github.com/thomasschafer/scooter.git/releases/download/v0.1.0/libscooter_hx.so")))))
