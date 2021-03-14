(define-module (deck matrix-client)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (deck core types state)
  #:use-module (deck core types filter)
  #:use-module (deck core session)
  #:export (<matrix-client>
            matrix-client?
            matrix-client-session
            matrix-client-session-set!
            matrix-client-callbacks
            matrix-client-callbacks-set!
            matrix-client-presence-callbacks
            matrix-client-presence-callbacks-set!
            matrix-client-rooms-callbacks
            matrix-client-rooms-callbacks-set!
            matrix-client-ephemeral-callbacks
            matrix-client-ephemeral-callbacks-set!

            matrix-client-add-callback!
            matrix-client-add-presence-callback!
            matrix-client-add-invite-callback!
            matrix-client-add-ephemeral-callback!

            matrix-client-start!))



;; Default filter for synchronization.
(define %default-filter
  (make <filter>
    #:room (make <room-filter>
             #:timeline (make <event-filter>
                          #:limit 10))))


;; This class describes a Matrix asynchronous client.

(define-class <matrix-client> ()
  ;; REQUIRED.
  ;;
  ;; <session>
  (session
   #:init-value   #f
   #:init-keyword #:session
   #:getter       matrix-client-session
   #:setter       matrix-client-session-set!)

  ;; <thread>
  (sync-thread
   #:init-value   #f
   #:getter       matrix-client-sync-thread
   #:setter       matrix-client-sync-thread-set!)

  (sync-timeout
   #:init-value   30000
   #:init-keyword #:sync-timeout
   #:getter       matrix-client-sync-timeout)

  ;; <string>
  (sync-token
   #:init-value   #f
   #:getter       matrix-client-sync-token
   #:setter       matrix-client-sync-token-set!)

  ;; <string> or <filter>
  (sync-filter
   #:init-value   %default-filter
   #:init-keyword #:sync-filter
   #:getter       matrix-client-sync-filter)

  ;; <list> of <procedure>
  (callbacks
   #:init-value   '()
   #:init-keyword #:callbacks
   #:getter       matrix-client-callbacks
   #:setter       matrix-client-callbacks-set!)

  ;; <list> of <procedure>
  (presence-callbacks
   #:init-value   '()
   #:init-keyword #:presence-callbacks
   #:getter       matrix-client-presence-callbacks
   #:setter       matrix-client-presence-callbacks-set!)

  ;; <list> of <procedure>
  (rooms-callbacks
   #:init-value   '()
   #:init-keyword #:rooms-callbacks
   #:getter       matrix-client-rooms-callbacks
   #:setter       matrix-client-rooms-callbacks-set!)

  ;; <list> of <procedure>
  (ephemeral-callbacks
   #:init-value   '()
   #:init-keyword #:ephemeral-callbacks
   #:getter       matrix-client-ephemeral-callbacks
   #:setter       matrix-client-ephemeral-callbacks-set!))

(define-method (initialize (matrix-client <matrix-client>) initargs)
  (next-method)
  (let ((session (and (memq #:session initargs)
                      (cadr (memq #:session initargs)))))

    (unless session
      (error "No session provided"))))



;; Check if an OBJECT is a <matrix-client> instance.
(define-method (matrix-client? object)
  (is-a? <matrix-client> object))



(define-method (matrix-client-add-callback! (matrix-client <matrix-client>)
                                            (callback      <procedure>))
  (matrix-client-callbacks-set!
   matrix-client
   (cons callback (matrix-client-callbacks matrix-client))))

(define-method (matrix-client-add-presence-callback! (matrix-client <matrix-client>)
                                                     (callback      <procedure>))
  (matrix-client-presence-callbacks-set!
   matrix-client
   (cons callback (matrix-client-presence-callbacks matrix-client))))

(define-method (matrix-client-add-rooms-callback! (matrix-client <matrix-client>)
                                                  (callback      <procedure>))
  (matrix-client-rooms-callbacks-set!
   matrix-client
   (cons callback (matrix-client-rooms-callbacks matrix-client))))

(define-method (matrix-client-add-ephemeral-callback! (matrix-client <matrix-client>)
                                                      (callback      <procedure>))
  (matrix-client-ephemeral-callbacks-set!
   matrix-client
   (cons callback (matrix-client-ephemeral-callbacks matrix-client))))



;; The main loop of the client.
(define-method (matrix-client-main-loop (matrix-client <matrix-client>)
                                        (timeout       <number>))
  (let* ((session (matrix-client-session matrix-client))
         (token   (matrix-client-sync-token matrix-client))
         (filter  (matrix-client-sync-filter matrix-client))
         (state   (session-sync session
                                #:since  token
                                #:filter filter)))
    (matrix-client-sync-token-set! matrix-client (state-next-batch state))
    (let ((events  (state-presense-events state)))
      ;; (format #t "presence: ~a~%" presence)
      (unless (zero? (vector-length events))
        (for-each (lambda (event)
                    (for-each (lambda (proc) (proc event))
                              (matrix-client-presence-callbacks matrix-client)))
                  (vector->list events))))
    (let ((invite      (state-rooms-invite state))
          (join        (state-rooms-join state))
          (leave       (state-rooms-leave state)))
      ;; (format #t "rooms-state: ~a~%" rooms-state)
      (unless (and (null? invite)
                   (null? join)
                   (null? leave))
        (for-each (lambda (proc) (proc invite join leave))
                  (matrix-client-rooms-callbacks matrix-client))))
    (usleep timeout)
    (matrix-client-main-loop matrix-client timeout)))

;; Start the main loop of the client in a separate thread.
(define-method (matrix-client-start! (matrix-client <matrix-client>))
  (let* ((timeout (matrix-client-sync-timeout matrix-client))
         (thread  (call-with-new-thread
                   (lambda ()
                     (matrix-client-main-loop matrix-client timeout)))))
    (matrix-client-sync-thread-set! matrix-client thread)))

