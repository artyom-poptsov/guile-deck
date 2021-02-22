(define-module (deck core net client)
  #:use-module (srfi srfi-8)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json)
  #:export (<client>
            client-server-uri
            client-get
            client-put
            client-post
            uri-parameters->string))


(define-class <client> ()
  ;; <uri>
  (server-uri
   #:init-value   #f
   #:init-keyword #:server
   #:getter       client-server-uri))



(define-method (display (client <client>) (port <port>))
  (format port "#<client ~a ~a>"
          (client-server-uri client)
          (number->string (object-address pipe) 16)))

(define-method (write (client <client>) (port <port>))
  (display client port))

(define-method (display (client <client>))
  (next-method)
  (display client (current-output-port)))

(define-method (write (client <client>))
  (next-method)
  (display client (current-output-port)))



(define-method (uri-parameters->string (params <list>))
  (if (null? params)
      ""
      (string-join (map (lambda (p)
                          (string-append (car p) "=" (cdr p)))
                        params)
                   "&")))


(define* (client-get client
                     resource
                     #:key
                     (query '()))
  (let* ((server (client-server-uri client))
         (uri    (build-uri (uri-scheme server)
                            #:host   (uri-host server)
                            #:port   (uri-port server)
                            #:path   resource
                            #:query  (uri-parameters->string query))))
    (receive (response body)
        (http-get uri)
      (and (= (response-code response) 200)
           (json-string->scm (bytevector->string body "UTF-8"))))))

(define* (client-put client resource body
                      #:key
                      (query '()))
  (let* ((server (client-server-uri client))
         (uri    (build-uri (uri-scheme server)
                            #:host   (uri-host server)
                            #:port   (uri-port server)
                            #:path   resource
                            #:query  (uri-parameters->string query)))
         (json-body (scm->json-string body)))
    (receive (response response-body)
        (http-put uri
                  #:headers '((Content-Type . "application/json"))
                  #:port    (open-socket-for-uri uri)
                  #:body    json-body)
      (display response)
      (newline)
      (json-string->scm (bytevector->string response-body "UTF-8")))))

(define* (client-post client resource body
                      #:key
                      (query '()))
  (let* ((server (client-server-uri client))
         (uri    (build-uri (uri-scheme server)
                            #:host   (uri-host server)
                            #:port   (uri-port server)
                            #:path   resource
                            #:query  (uri-parameters->string query)))
         (json-body (scm->json-string body)))
    (receive (response response-body)
        (http-post uri
                   #:headers '((Content-Type . "application/json"))
                   #:port    (open-socket-for-uri uri)
                   #:body    json-body)
      (display response)
      (newline)
      (json-string->scm (bytevector->string response-body "UTF-8")))))
