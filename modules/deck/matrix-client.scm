(define-module (deck matrix-client)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 hash-table)
  #:use-module (deck core types state)
  #:use-module (deck core types filter)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core room)
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
            matrix-client-room
            matrix-client-rooms

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

  ;; <hash-table>
  (rooms
   #:init-value   (make-hash-table 10)
   #:getter       matrix-client-rooms)

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

  (on-timeline-event-callbacks
   #:init-value   (make-hash-table)
   #:init-keyword #:on-timeline-event
   #:getter       matrix-client-on-timeline-event
   #:setter       matrix-client-on-timeline-event-set!)

  ;; (on-state-event-callbacks
  ;;  #:init-value   (make-hash-table)
  ;;  #:getter       matrix-client-on-state-event-callbacks)

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
      (error "No session provided")))

  (let ((on-invite (and (memq #:on-invite initargs)
                        (cadr (memq #:on-invite initargs)))))
    (when on-invite
      (cond
       ((procedure? on-invite)
        (slot-set! matrix-client 'on-invite-callbacks (list on-invite)))
       ((list? on-invite)
        (slot-set! matrix-client 'on-invite-callbacks on-invite))
       (else
        (error "#:on-invite must be a <list> of <procedure> or a <procedure>")))))

  (let ((on-update (and (memq #:on-update initargs)
                        (cadr (memq #:on-update initargs)))))
    (when on-update
      (cond
       ((procedure? on-update)
        (slot-set! matrix-client 'on-update-callbacks (list on-update)))
       ((list? on-update)
        (slot-set! matrix-client 'on-update-callbacks on-update))
       (else
        (error "#:on-update must be a <list> of <procedure> or a <procedure>")))))

  (let ((on-leave (and (memq #:on-leave initargs)
                       (cadr (memq #:on-leave initargs)))))
    (when on-leave
      (cond
       ((procedure? on-leave)
        (slot-set! matrix-client 'on-leave-callbacks (list on-leave)))
       ((list? on-leave)
        (slot-set! matrix-client 'on-leave-callbacks on-leave))
       (else
        (error "#:on-leave must be a <list> of <procedure> or a <procedure>")))))

  (let ((on-timeline-event (and (memq #:on-timeline-event initargs)
                                (cadr (memq #:on-timeline-event initargs)))))
    (when on-timeline-event
      (cond
       ((list? on-timeline-event)
        (matrix-client-on-timeline-event-set!
         matrix-client
         (alist->hash-table on-timeline-event)))
       ((hash-table? on-timeline-event)
        (matrix-client-on-timeline-event-set! matrix-client
                                              on-timeline-event))
       (else
        (error "#:on-timeline-event must be an associative list or a hash table"
               on-timeline-event))))))



;; Check if an OBJECT is a <matrix-client> instance.
(define-method (matrix-client? object)
  (is-a? <matrix-client> object))

;; Get a <room> instance by its ROOM-ID from the internal <matrix-client> hash
;; table of rooms.
(define-method (matrix-client-room (client <matrix-client>) (room-id <string>))
  (hash-ref (matrix-client-rooms client) room-id))

;; The same as above, only takes a ROOM-ID as a <matrix-id> instead of a
;; <string>.
(define-method (matrix-client-room (client <matrix-client>) (room-id <matrix-id>))
  (matrix-client-room client (matrix-id->string room-id)))

;; Add a new room with ROOM-ID to the hash table of rooms if it is not present
;; there yet.
(define-method (matrix-client-room-add! (client <matrix-client>) (room-id <string>))
  (unless (hashq-ref (matrix-client-rooms client) room-id)
    (hash-set! (matrix-client-rooms client)
               room-id
               (make <room>
                 #:session (matrix-client-session client)
                 #:id      room-id))))

(define-method (matrix-client-room-add! (client <matrix-client>) (room-id <matrix-id>))
  (matrix-client-room-add! client (matrix-id->string room-id)))



;; The main loop of the client.
(define-method (matrix-client-main-loop (matrix-client <matrix-client>)
                                        (timeout       <number>))
  (define (handle-presence events)
    (for-each
     (lambda (event)
       (for-each (lambda (proc) (proc matrix-client event))
                 (matrix-client-presence-callbacks matrix-client)))
     (vector->list events)))

  (define (handle-invite updates)
    (for-each
     (lambda (update)
       (matrix-client-room-add! matrix-client (room-update-id update))
       (for-each (lambda (proc) (proc matrix-client update))
                 (matrix-client-on-invite matrix-client)))
     updates))

  (define (handle-updates updates)
    (for-each
     (lambda (update)
       (matrix-client-room-add! matrix-client (room-update-id update))
       (for-each (lambda (proc) (proc matrix-client update))
                 (matrix-client-on-update matrix-client))

       (let ((timeline  (room-update-content:timeline update))
             (callbacks (matrix-client-on-timeline-event matrix-client)))
         (when timeline
           (for-each
            (lambda (event)
              (let* ((type (assoc-ref event "type"))
                     (proc (hash-ref callbacks type)))
                (when (procedure? proc)
                  (proc matrix-client (room-update-id update) event))))
            (vector->list (timeline:events timeline))))))
     updates))

  (define (handle-leave updates)
    (for-each
     (lambda (update)
       (matrix-client-room-add! matrix-client (room-update-id update))
       (for-each (lambda (proc) (proc matrix-client update))
                 (matrix-client-on-leave matrix-client)))
     updates))

  (let* ((session (matrix-client-session matrix-client))
         (token   (matrix-client-sync-token matrix-client))
         (filter  (matrix-client-sync-filter matrix-client))
         (state   (session-sync session
                                #:since  token
                                #:filter filter)))
    (matrix-client-sync-token-set! matrix-client (state-next-batch state))

    (when (state-presense-events-available? state)
      (handle-presence (state-presense-events state)))

    (when (state-rooms-invite-available? state)
      (handle-invite (state-rooms-invite state)))

    (when (state-rooms-join-available? state)
      (handle-updates (state-rooms-join state)))

    (when (state-rooms-leave-available? state)
      (handle-leave (state-rooms-leave state)))

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

