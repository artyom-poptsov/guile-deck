(define-module (deck core net http-client)
  #:use-module (oop goops)
  #:use-module (web client)
  #:export (<http-client>
            http-client-server-url
            http-client-get
            http-client-put
            http-client-post
            uri-parameters->string))


(define-class <http-client> ()
  ;; <string>
  (server-url
   #:init-value   #f
   #:init-keyword #:server
   #:getter       http-client-server-url))


(define-method (uri-parameters->string (params <list>))
  (if (null? params)
      ""
      (string-append "?"
                     (string-join
                      (map (lambda (p)
                             (string-append (car p) "=" (cdr p)))
                           params)
                      "&"))))


(define-generic http-client-get)

(define-method (http-client-get (client   <http-client>)
                                (resource <string>)
                                (params   <list>)
                                (headers  <list>))
  (let* ((uri (string-append (http-client-server-url client)
                             resource))
         (uri (uri-parameters->string params)))
    (http-get uri #:headers headers)))

(define-method (http-client-get (client   <http-client>)
                                (resource <string>)
                                (params   <list>))
  (http-client-get client resource params '()))

(define-method (http-client-get (client   <http-client>)
                                (resource <string>))
  (http-client-get client resource '() '()))



(define-generic http-client-post)

(define-method (http-client-post (client   <http-client>)
                                 (resource <string>)
                                 (params   <list>)
                                 (headers  <list>))
  (let* ((uri (string-append (http-client-server-url client)
                             resource))
         (uri (uri-parameters->string params)))
    (http-post uri #:headers headers)))

(define-method (http-client-post (client   <http-client>)
                                 (resource <string>)
                                 (params   <list>))
  (http-client-post client resource params '()))

(define-method (http-client-post (client   <http-client>)
                                 (resource <string>))
  (http-client-post client resource '() '()))



(define-generic http-client-put)

(define-method (http-client-put (client   <http-client>)
                                (resource <string>)
                                (params   <list>)
                                (headers  <list>))
  (let* ((uri (string-append (http-client-server-url client)
                             resource))
         (uri (uri-parameters->string params)))
    (http-put uri #:headers headers)))

(define-method (http-client-put (client   <http-client>)
                                (resource <string>)
                                (params   <list>))
  (http-client-put client resource params '()))

(define-method (http-client-put (client   <http-client>)
                                (resource <string>))
  (http-client-put client resource '() '()))



