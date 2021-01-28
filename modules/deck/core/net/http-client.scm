(define-module (deck core net http-client)
  #:use-module (oop goops)
  #:use-module (web client)
  #:export (<http-client>
            http-client-server-url
            http-client-get
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

(define-method (http-client-get (client   <http-client>)
                                (resource <string>)
                                (params   <list>)
                                (headers  <list>))
  (let* ((uri (string-append (http-client-server-url client)
                             resource))
         (uri (uri-parameters->string params)))
    (http-get uri #:headers headers)))


