;; websocket server demo
(library (websocket server)
  (export
    websocket-server-create
    websocket-server-close)
  (import
    (scheme)
    (socket socket)
    (websocket base64)
    (websocket http-cgi))

  (define threads?
    (char=? #\t (string-ref (symbol->string (machine-type)) 0)))

  (define websocket-server-create
    (case-lambda
      ((port)
        (websocket-server-create "127.0.0.1" port))
      ((ip port)
        (let* ([family AF_INET]
               [server (socket:socket family SOCK_STREAM IPPROTO_IP)])
          (socket:bind server family ip port)
          (socket:listen server)
          (let loop ([client (socket:accept server)])
            (when (>= client 0)
              (if threads?
                (fork-thread
                  (lambda ()
                    (server-proc client)))
                (server-proc client))
              (loop (socket:accept server))))))))
         
  (define websocket-server-close
    (lambda (server)
      (socket:close server)
      (socket:cleanup)))

  (define server-proc
    (lambda (socket)
      (let loop ()
        (let ([res (utf8->string (socket:read socket))])
        (display res)
          (printf "from-web:~a\n" res)
          (printf "headers-web:~a\n" 
            (http:read-header (open-input-string res)))
          
          (socket:write socket (string->utf8 response-headers))
        ; (let ([event-type (get-event-type socket)])
        ;   (cond
        ;     [(string=? event-type "accept")
        ;       (display (utf-8->string (event-type)))]
        ;     [(string=? event-type "read")
        ;     (handle-message (get-event-ident n))]
        ;     [(string=? event-type "disconnect")
        ;     (check 'close (close (get-event-ident n)))]
        ;     [else (display "other event")]))
        (loop))
        )))

  (define handle-message
    (lambda (socket)
      (printf "~a--~a\n" socket (socket:read socket))
      (let [(response-data response-headers)]
        (socket:write socket (string->utf8 response-headers))
      )))

  (define response-headers
    (apply
      string-append
      (list
        "HTTP/1.1 101 Web Socket Protocol Handshake\r\n"
        "Access-Control-Allow-Credentials: true\r\n"
        "Access-Control-Allow-Headers: content-type\r\n"
        "Access-Control-Allow-Headers: authorization\r\n"
        "Access-Control-Allow-Headers: x-websocket-extensions\r\n"
        "Access-Control-Allow-Headers: x-websocket-version\r\n"
        "Access-Control-Allow-Headers: x-websocket-protocol\r\n"
        "Connection: Upgrade\r\n"
        "Date: Wed, 08 Jun 2016 14:15:44 GMT\r\n"
        "Sec-WebSocket-Accept: tmpHZWtv6raspXQTTbcwpkfE924=\r\n"
        "Server: Kaazing Gateway\r\n"
        "Upgrade: websocket\r\n\r\n"
      ))
    )
)