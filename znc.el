;;; znc.el -- ZNC + ERC 

;;; Heleprs
(defun znc-kill-buffer-always (&optional buffer)
  "Murderface a buffer, don't listen to nobody, son!"
  (interactive "b")
  (let ((buffer (or buffer (current-buffer)))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defvar *znc-server-default-host* "localhost"
  "Default host to use in `*znc-server-default*'")

(defvar *znc-server-default-port* 12533
  "Default port to use in `*znc-server-default*'")

(defconst *znc-server-accounts-type* '((symbol :tag "Network Slug" :value network-slug)
                                       (group (string :tag "Username"     :value "znc-username")
                                              (string :tag "Password"     :value "znc-password")))
  "A group describing an account belonging to a server")

(defconst *znc-server-type* `(list :tag "Server"
                                    (group (string  :tag "Host" :value ,*znc-server-default-host*)
                                           (integer :tag "Port" :value ,*znc-server-default-port*)
                                           (repeat (cons :tag "Accounts" 
                                                         ,@*znc-server-accounts-type*))))
  "A group describing a ZNC server endpoint and the accounts on it")

;;; Custom
(defgroup znc nil
  "ZNC IRC Bouncer assistance and opinions"
  :group 'erc)


(defcustom znc-servers nil
  "List of ZNC servers"
  :group 'znc
  :type `(repeat ,*znc-server-type*))

(provide 'znc)
