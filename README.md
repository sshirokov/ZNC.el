#### It's a ZNC "mode"
[![MELPA](https://melpa.org/packages/znc-badge.svg)](https://melpa.org/#/znc)

Aint that exciting?

#### Quickstart 

```sh
$ git clone https://github.com/sshirokov/ZNC.el
```

Make sure the contents of the repo, or just [znc.el](/znc.el) is on your load-path.
Then add this to your `init.el` or equivalent

```elisp
;; Install
(require 'znc)
```

Configure it for your specific instance from within emacs once installed.

```
M-x customize-group znc RET
```

alternatively, put this in your init.el/configuration file..

```
(require 'znc)
(setq znc-servers
    '(("example.com" PORT# t
      ((NETWORK-SLUG "USERNAME" "PASSWORD")
       (NETWORK-SLUG "USERNAME" "PASSWORD"))
)))

```
where example.com is your ZNC bouncer address/hostname and PORT# is the port # that the bouncer is listening on.
NETWORK-SLUG would be freenode or whatever you defined in your znc as the network name.
USERNAME and PASSWORD are the username and password you authenticate to ZNC with.


Engage!
It'll prompt you for a configured server, or tell you to configure one

```
M-x znc-erc

```

Or you can make a giant sweep and connect to freaking everything

```
M-x znc-all
```

#### Usage notes

`/reconnect` will recycle the hell out of the server buffer.

By default, when you kill a channel buffer instead of `/part`ing from the channel, you will `/detach` from it.
This is customizable. 

You can also get ZNC.el from the [MELPA](https://melpa.org/#/znc) ELPA repository. See https://melpa.org/#/getting-started for instructions on using it.
