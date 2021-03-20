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
            matrix-client-on-invite
            matrix-client-on-update
            matrix-client-on-leave
            matrix-client-ephemeral-callbacks
            matrix-client-ephemeral-callbacks-set!

            matrix-client-start!
            matrix-client-stop!))



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

  ;; Synchronization thread.
  ;;
  ;; <thread>
  (sync-thread
   #:init-value   #f
   #:getter       matrix-client-sync-thread
   #:setter       matrix-client-sync-thread-set!)

  ;; The client will be checking the server for updates once per this timeout.
  ;;
  ;; <number>
  (sync-timeout
   #:init-value   30000
   #:init-keyword #:sync-timeout
   #:getter       matrix-client-sync-timeout)

  ;; A point in time to continue a sync from.
  ;;
  ;; <string>
  (sync-token
   #:init-value   #f
   #:getter       matrix-client-sync-token
   #:setter       matrix-client-sync-token-set!)

  ;; The ID of a filter created using the filter API or a <filter> object.
  ;; Passing the filter object is best suited to one off requests. Creating a
  ;; filter using the filter API is recommended for clients that reuse the
  ;; same filter multiple times, for example in long poll requests.
  ;;
  ;; <string> or <filter>
  (sync-filter
   #:init-value   %default-filter
   #:init-keyword #:sync-filter
   #:getter       matrix-client-sync-filter)

  ;; <list> of <procedure>
  (presence-callbacks
   #:init-value   '()
   #:init-keyword #:presence-callbacks
   #:getter       matrix-client-presence-callbacks
   #:setter       matrix-client-presence-callbacks-set!)

  ;; Callbacks that handle room invites.
  ;;
  ;; <list> of <procedure> | <procedure>
  (on-invite-callbacks
   #:init-value   '()
   #:init-keyword #:on-invite
   #:getter       matrix-client-on-invite)

  ;; Callbacks that handle updates to joined rooms.
  ;;
  ;; <list> of <procedure> | <procedure>
  (on-update-callbacks
   #:init-value   '()
   #:init-keyword #:on-update
   #:getter       matrix-client-on-update)

  ;; Callbacks that called when the current user left a room.
  ;;
  ;; <list> of <procedure> | <procedure>
  (on-leave-callbacks
   #:init-value   '()
   #:init-keyword #:on-leave
   #:getter       matrix-client-on-leave)

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
    (when (state-presense-events-available? state)
      (for-each (lambda (event)
                  (for-each (lambda (proc) (proc event))
                            (matrix-client-presence-callbacks matrix-client)))
                (vector->list (state-presense-events state))))

    (when (state-rooms-invite-available? state)
      (let ((updates (state-rooms-invite state)))
        (for-each (lambda (update)
                    (for-each (lambda (proc) (proc update))
                              (matrix-client-on-invite matrix-client)))
                  updates)))

    (when (state-rooms-join-available? state)
      (let ((updates (state-rooms-join state)))
        (for-each (lambda (update)
                    (for-each (lambda (proc) (proc update))
                              (matrix-client-on-update matrix-client)))
                  updates)))

    (when (state-rooms-leave-available? state)
      (let ((updates (state-rooms-leave state)))
        (for-each (lambda (udpate)
                    (for-each (lambda (proc) (proc leave))
                              (matrix-client-on-leave matrix-client)))
                  updates)))

    (usleep timeout)
    (matrix-client-main-loop matrix-client timeout)))

;; Start the main loop of the client in a separate thread.
(define-method (matrix-client-start! (matrix-client <matrix-client>))
  (let* ((timeout (matrix-client-sync-timeout matrix-client))
         (thread  (call-with-new-thread
                   (lambda ()
                     (matrix-client-main-loop matrix-client timeout)))))
    (matrix-client-sync-thread-set! matrix-client thread)))

(define-method (matrix-client-stop! (matrix-client <matrix-client>))
  (cancel-thread (matrix-client-sync-thread matrix-client))
  (join-thread (matrix-client-sync-thread matrix-client)))

