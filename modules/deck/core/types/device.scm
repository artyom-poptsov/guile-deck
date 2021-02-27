(define-module (deck core types device)
  #:use-module (oop goops)
  #:export (<device>
            device-id
            device-display-name
            device-last-seen-ip
            device-last-seen-timestamp))


;; Description:
;;   <https://matrix.org/docs/api/client-server/#!/Device32management/getDevices>

(define-class <device> ()
  ;; REQUIRED.  Identifier of this device.
  ;;
  ;; <string>
  (device-id
   #:init-value   #f
   #:init-keyword #:id
   #:getter       device-id)

  ;; Display name set by the user for this device. Absent if no name has been
  ;; set.
  ;;
  ;; <string>
  (display-name
   #:init-value   #f
   #:init-keyword #:display-name
   #:getter       device-display-name)

  ;; The IP address where this device was last seen. (May be a few minutes out
  ;; of date, for efficiency reasons).
  ;;
  ;; <string>
  (last-seen-ip
   #:init-value   #f
   #:init-keyword #:last-seen-ip
   #:getter       device-last-seen-ip)

  ;; The timestamp (in milliseconds since the unix epoch) when this devices
  ;; was last seen. (May be a few minutes out of date, for efficiency
  ;; reasons).
  ;;
  ;; <number>
  (last-seen-timestamp
   #:init-value   #f
   #:init-keyword #:last-seen-timestamp
   #:getter       device-last-seen-timestamp))



(define-method (display (device <device>) (port <port>))
  (format port "#<device ~a ~a>"
          (device-id device)
          (number->string (object-address pipe) 16)))

(define-method (write (device <device>) (port <port>))
  (display device port))

(define-method (display (device <device>))
  (next-method)
  (display device (current-output-port)))

(define-method (write (device <device>))
  (next-method)
  (display device (current-output-port)))



;;; device.scm ends here.
