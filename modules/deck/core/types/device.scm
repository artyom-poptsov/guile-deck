(define-module (deck core types device)
  #:use-module (oop goops)
  #:export (<device>
            device?
            device-id
            device-display-name
            device-last-seen-ip
            device-last-seen-timestamp
            alist->device))


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



(define (device? object)
  (is-a? object <device>))



(define-method (equal? (d1 <device>) (d2 <device>))
  (and (equal? (device-id d1) (device-id d2))
       (equal? (device-display-name d1) (device-display-name d2))
       (equal? (device-last-seen-ip d1) (device-last-seen-ip d2))
       (equal? (device-last-seen-timestamp d1) (device-last-seen-timestamp d2))))



;; Convert an alist to a <device> instance.
(define-method (alist->device (alist <list>))
  (make <device>
    #:id (assoc-ref alist "device_id")
    #:display-name (assoc-ref alist "display_name")
    #:last-seen-ip (assoc-ref alist "last_seen_ip")
    #:last-seen-timestamp (assoc-ref alist "last_seen_ts")))

;;; device.scm ends here.
