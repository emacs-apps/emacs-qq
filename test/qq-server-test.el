;;; qq-server-test.el --- QQ transport adapter tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'qq-server)

(ert-deftest qq-server-connect-configures-only-its-own-client ()
  (let ((qq-server--client (appkit-gateway-create))
        (other (appkit-gateway-create :websocket-url "ws://other/"))
        (qq-server-websocket-url "ws://localhost:3002/")
        (qq-server-auth-token-file "/tmp/example-token")
        connected)
    (cl-letf (((symbol-function 'appkit-gateway-start)
               (lambda (client) (setq connected client))))
      (qq-server-start))
    (should (eq connected qq-server--client))
    (should (= (appkit-gateway--protocol-version qq-server--client) 29))
    (should (equal (appkit-gateway--websocket-url qq-server--client) qq-server-websocket-url))
    (should (equal (appkit-gateway--auth-token-file qq-server--client) qq-server-auth-token-file))
    (should (equal (appkit-gateway--websocket-url other) "ws://other/"))))

(ert-deftest qq-server-send-delegates-and-retains-wire-result ()
  (let ((qq-server--client (appkit-gateway-create)) args result
        (qq-server-request-timeout 17))
    (cl-letf (((symbol-function 'appkit-gateway-send)
               (lambda (client method params success &rest _)
                 (setq args (list client method params))
                 (funcall success '((items . [])))
                 "request-1")))
      (should (equal (qq-server-send "account.list" nil
                                    (lambda (value) (setq result value))) "request-1")))
    (should (eq (car args) qq-server--client))
    (should (= (appkit-gateway--timeout qq-server--client) 17))
    (should (equal result '((items . []))))))

(provide 'qq-server-test)
