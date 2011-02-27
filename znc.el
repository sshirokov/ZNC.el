;;; znc.el -- ZNC + ERC
(require 'cl)
(require 'erc)

(defgroup znc nil
  "ZNC IRC Bouncer assistance and opinions.

This is a thin wrapper around `erc' that makes using
the ZNC (http://en.znc.in/) IRC bouncer and irons out
some of the quirks that arise from using it with a naive ERC. "
  :group 'erc)

;; Default vars
(defvar *znc-server-default-host* "localhost" "Default host to use in `*znc-server-default*'")
(defvar *znc-server-default-port* 12533 "Default port to use in `*znc-server-default*'")

;; Types
(defconst *znc-server-accounts-type* '((symbol :tag "Network Slug" :value network-slug)
                                       (group (string :tag "Username"     :value "znc-username")
                                              (string :tag "Password"     :value "znc-password")))
  "A group describing an account belonging to a server")

(defconst *znc-server-type* `(group (string  :tag "Host" :value ,*znc-server-default-host*)
                                    (integer :tag "Port" :value ,*znc-server-default-port*)
                                    (repeat (cons :tag "Accounts"
                                                  ,@*znc-server-accounts-type*)))
  "A group describing a ZNC server endpoint and the accounts on it")

;; Customizations
(defcustom znc-servers nil
  "List of ZNC servers"
  :group 'znc
  :type `(repeat ,*znc-server-type*))


(defun* znc-walk-all-servers (&key (each (lambda (&rest r) (mapcar 'identity r)))
                                   (pred (lambda (&rest _) t))
                                   (first nil))
  "Walk ever defined server and user pair calling `each' every time `pred' is non-nil

Both functions are called as: (apply f slug host port user pass)
`each' defaults to (mapcar 'identity ..)
`pred' is a truth function
`first' if non-nil, return the car of the result"
    (funcall (if first 'car 'identity)
             (loop for (host port users) in znc-servers
                   appending (loop for (slug user pass) in users collecting
                     `(,slug ,host ,port ,user ,pass)) into endpoints
                   finally return (loop for endpoint in endpoints
                     if (apply pred endpoint)
                     collect (apply each endpoint)))))

(defun znc-detach-channel ()
    (when (erc-server-process-alive)
    (let ((tgt (erc-default-target)))
      (erc-server-send (format "DETACH %s" tgt)
		       nil tgt))))

(defun znc-set-name (znc-name &optional buffer)
  "Set the znc-buffer-name buffer local to znc-name in buffer or (current-buffer)"
  (let ((buffer (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer buffer
      (make-local-variable 'znc-buffer-name)
      (setf znc-buffer-name znc-name))))

(defun znc-erc-connect (network erc-args)
  (let ((buffer (format "*irc-%s*" network))
        (erc-buffer (apply 'erc erc-args)))
    (when (get-buffer buffer)
      (znc-kill-buffer-always buffer))
    (znc-set-name buffer erc-buffer)
    (with-current-buffer erc-buffer
      (rename-buffer buffer))))

(defmacro with-endpoint (endpoint &rest forms)
  "Wraps the remainder in a binding in which
`slug' `host' `port' `user' `pass' are bound 
to the matching values for the endpoint"
  (let ((sympoint (gensym "endpoint")))
    `(let ((,sympoint ,endpoint))
       (destructuring-bind (slug host port user pass) ,sympoint
         ,@forms))))

(defun znc-erc (&optional network)
  (interactive)
  (let* ((networks (znc-walk-all-servers :each 'znc-endpoint-slug-name))
         (network (when networks
                    (znc-prompt-string-or-nil "Network" networks (car networks) t)))
         (endpoint (when network
                     (znc-walk-all-servers :first t :pred (znc-walk-slugp (read network))))))
        (if endpoint
            (with-endpoint endpoint
                           (znc-erc-connect slug
                                            `(:server ,host :port ,port
                                              :nick ,user :password ,(format "%s:%s" user pass))))
          (message "Network %s not defined. Try M-x customize-group znc."
                   (symbol-name network)))))

;; Advice
(defadvice erc-server-reconnect (after znc-erc-rename last nil activate)
  "Maybe rename the buffer we create"
  (let* ((wants-name (and (local-variable-p 'znc-buffer-name (erc-server-buffer))
                          (buffer-local-value 'znc-buffer-name (erc-server-buffer))))
         (current (erc-server-buffer))
         (returning ad-return-value))
    (if wants-name
        (progn
          (ignore-errors (znc-kill-buffer-always wants-name))
          (with-current-buffer returning
            (znc-set-name wants-name)
            (rename-buffer wants-name))
          (get-buffer wants-name))
      returning)))

;;; Heleprs
(defun znc-kill-buffer-always (&optional buffer)
  "Murderface a buffer, don't listen to nobody, son!"
  (interactive "b")
  (let ((buffer (or buffer (current-buffer)))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun znc-walk-slugp (slug)
  (lexical-let ((slug slug))
    (lambda (s &rest _) (eq s slug))))

(defun znc-endpoint-slug-name (&rest args)
  (symbol-name (apply 'znc-endpoint-slug args)))
(defun znc-endpoint-slug (s &rest _) s)

(defun znc-prompt-string-or-nil (prompt &optional completions default require-match)
  (let* ((string (completing-read (concat prompt ": ") completions nil require-match default))
         (string (if (equal string "") nil string)))
    string))


;;;;;;;;;;;;;;;;;;;
;; Provide!     ;;;
(provide 'znc)  ;;;
;;;;;;;;;;;;;;;;;;;
