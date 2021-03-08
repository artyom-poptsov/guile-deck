;; Description:
;;   <https://matrix.org/docs/api/client-server/#!/Room32participation/defineFilter>

(define-module (deck core types filter)
  #:use-module (oop goops)
  #:export (<filter>
            ;; filter?
            ;; alist->filter

            <event-filter>
            ;; event-filter?
            ;; alist->event-filter

            <room-filter>
            ;; room-filter?
            ;; alist->room-filter

            <state-filter>
            ))


(define-class <event-filter> ()
  ;; <number>
  (limit
   #:init-value   #f
   #:init-keyword #:limit
   #:getter       event-filter-limit)

  ;; <list> of <string>
  (not-senders
   #:init-value   #f
   #:init-keyword #:not-senders
   #:getter       event-filter-not-senders)

  ;; <list> of <string>
  (not-types
   #:init-value   #f
   #:init-keyword #:not-types
   #:getter       event-filter-not-types)

  ;; <list> of <string>
  (senders
   #:init-value   #f
   #:init-keyword #:senders
   #:getter       event-filter-senders)

  ;; <list> of <string>
  (types
   #:init-value   #f
   #:init-keyword #:types
   #:getter       event-filter-types))


(define-class <room-filter> ()
  ;; <event-filter>
  (account-data
   #:init-value   #f
   #:init-keyword #:account-data
   #:getter       room-filter-account-data)

  ;; <event-filter>
  (ephemeral
   #:init-value   #f
   #:init-keyword #:ephemeral
   #:getter       room-filter-ephemeral)

  ;; <boolean>
  (include-leave?
   #:init-value   #f
   #:init-keyword #:include-leave?
   #:getter       room-filter-include-leave?)

  ;; <list> of <string>
  (not-rooms
   #:init-value   #f
   #:init-keyword #:not-rooms
   #:getter       room-filter-not-rooms)

  (rooms
   #:init-value   #f
   #:init-keyword #:rooms
   #:getter       room-filter-rooms)

  ;; <state-filter>
  (state
   #:init-value   #f
   #:init-keyword #:state
   #:getter       room-filter-state)

  (timeline
   #:init-value   #f
   #:init-keyword #:state
   #:getter       room-filter-timeline))



(define-class <state-filter> ()
  ;; <number>
  (limit
   #:init-value   #f
   #:init-keyword #:limit
   #:getter       state-filter-limit)

  ;; <list> of <string>
  (not-senders
   #:init-value   #f
   #:init-keyword #:not-senders
   #:getter       state-filter-not-senders)

  ;; <list> of <string>
  (not-types
   #:init-value   #f
   #:init-keyword #:not-types
   #:getter       state-filter-not-types)

  ;; <list> of <string>
  (senders
   #:init-value   #f
   #:init-keyword #:senders
   #:getter       state-filter-senders)

  ;; <list> of <string>
  (types
   #:init-value   #f
   #:init-keyword #:types
   #:getter       state-filter-types)

  ;; <boolean>
  (contains-url?
   #:init-value   #f
   #:init-keyword #:contains-url
   #:getter       state-filter-contains-url?)

  ;; <boolean>
  (include-redundant-members?
   #:init-value   #f
   #:init-keyword #:include-redundant-members?
   #:getter       state-filter-include-redundant-members?)

  ;; <boolean>
  (lazy-load-members?
   #:init-value   #f
   #:init-keyword #:lazy-load-members?
   #:getter       state-filter-lazy-load-members?)

  ;; <list> of <string>
  (not-rooms
   #:init-value   #f
   #:init-keyword #:not-rooms
   #:getter       state-filter-not-rooms)

  (rooms
   #:init-value   #f
   #:init-keyword #:rooms
   #:getter       state-filter-rooms))


(define-class <filter> ()
  ;; <event-filter>
  (account-data
   #:init-value   #f
   #:init-keyword #:account-data
   #:getter       filter-account-data)

  ;; <list> of <string>
  (event-fields
   #:init-value   #f
   #:init-keyword #:event-fields
   #:getter       filter-event-fields)

  ;; <string>
  (event-format
   #:init-value   #f
   #:init-keyword #:event-format
   #:getter       filter-event-format)

  ;; <event-filter>
  (presence
   #:init-value   #f
   #:init-keyword #:presense
   #:getter       filter-presence)

  ;; <room-filter>
  (room
   #:init-value   #f
   #:init-keyword #:room
   #:getter       filter-room))
