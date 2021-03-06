(define-module (deck core types state)
  #:use-module (oop goops)
  #:export (<state>
            state?
            state-account-data
            state-device-lists
            state-device-one-time-keys-count
            state-next-batch
            state-presense
            state-rooms
            alist->state))


;; See <https://matrix.org/docs/api/client-server/#!/Room32participation/sync>
(define-class <state> ()
  ;; The global private data created by this user.
  (account-data
   #:init-keyword #:account-data
   #:getter       state-account-data)

  ;; Information on end-to-end device updates.
  (device-lists
   #:init-keyword #:device-lists
   #:getter       state-device-lists)

  ;; Information on end-to-end encryption keys.
  (device-one-time-keys-count
   #:init-keyword #:device-one-time-keys-count
   #:getter        state-device-one-time-keys-count)

  ;; REQUIRED. The batch token to supply in the since param of the next /sync
  ;; request.
  ;;
  ;; <string>
  (next-batch
   #:init-keyword  #:next-batch
   #:getter        state-next-batch)

  ;; The updates to the presence status of other users.
  (presense
   #:init-keyword  #:presense
   #:getter        state-presense)

  ;; Updates to rooms.
  (rooms
   #:init-keyword  #:rooms
   #:getter        state-rooms)

  ;; Information on the send-to-device messages for the client device.
  (to-device
   #:init-keyword #:to-device
   #:getter       state-to-device))



(define-method (state? object)
  (is-a? <state> object))



(define-method (display (state <state>) (port <port>))
  (format port "#<state next-batch: ~a ~a>"
          (state-next-batch state)
          (number->string (object-address pipe) 16)))

(define-method (write (state <state>) (port <port>))
  (display state port))

(define-method (display (state <state>))
  (next-method)
  (display state (current-output-port)))

(define-method (write (state <state>))
  (next-method)
  (display state (current-output-port)))



(define-method (alist->state (alist <list>))
  (make <state>
    #:account-data (assoc-ref alist "account_data")
    #:device-lists (assoc-ref alist "device_lists")
    #:device-one-time-keys-count (assoc-ref alist "device_one_time_keys_count")
    #:next-batch   (assoc-ref alist "next_batch")
    #:presense     (assoc-ref alist "presence")
    #:rooms        (assoc-ref alist "rooms")
    #:to-device    (assoc-ref alist "to_device")))

