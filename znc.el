;;; znc.el -- ZNC + ERC
(require 'cl)

;;; Heleprs
(defun znc-kill-buffer-always (&optional buffer)
  "Murderface a buffer, don't listen to nobody, son!"
  (interactive "b")
  (let ((buffer (or buffer (current-buffer)))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun* znc-walk-all-servers (&key (each (lambda (&rest r) (mapcar 'identity r)))
                                   (pred (lambda (&rest _) t)))
  "Walk ever defined server and user pair calling `each' every time `pred' is non-nil

Both functions are called as: (apply f slug host port user pass)
`each' defaults to (mapcar 'identity ..)
`pred' is a truth function"
    (loop for (host port users) in znc-servers
          appending (loop for (slug user pass) in users collecting
            `(,slug ,host ,port ,user ,pass)) into endpoints
          finally return (loop for endpoint in endpoints
            if (apply pred endpoint)
            collect (apply each endpoint))))

(defvar *znc-server-default-host* "localhost"
  "Default host to use in `*znc-server-default*'")

(defvar *znc-server-default-port* 12533
  "Default port to use in `*znc-server-default*'")

(defconst *znc-server-accounts-type* '((symbol :tag "Network Slug" :value network-slug)
                                       (group (string :tag "Username"     :value "znc-username")
                                              (string :tag "Password"     :value "znc-password")))
  "A group describing an account belonging to a server")

(defconst *znc-server-type* `(group (string  :tag "Host" :value ,*znc-server-default-host*)
                                    (integer :tag "Port" :value ,*znc-server-default-port*)
                                    (repeat (cons :tag "Accounts"
                                                  ,@*znc-server-accounts-type*)))
  "A group describing a ZNC server endpoint and the accounts on it")

;;; Custom
(defgroup znc nil
  "ZNC IRC Bouncer assistance and opinions.

This is a thin wrapper around `erc' that makes using
the ZNC (http://en.znc.in/) IRC bouncer and irons out
some of the quirks that arise from using it with a naive ERC. "
  :group 'erc)


(defcustom znc-servers nil
  "List of ZNC servers"
  :group 'znc
  :type `(repeat ,*znc-server-type*))

(provide 'znc)
