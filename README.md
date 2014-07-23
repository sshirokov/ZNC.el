#### It's a ZNC "mode"

Aint that exciting?

#### Quickstart 

```sh
$ git clone https://github.com/sshirokov/ZNC.el
```

Make sure the contents of the repo, or just ./znc.el is on your load-path.
Then add this to your `init.el` or equivalent

```elisp
;; Install
(require 'znc)
```

Configure it for your specific instance from within emacs once installed.

```
M-x customize-group znc RET
```

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

By default, when you kill a channel buffer instead of `/part`ing from the channel, you will `/detach` from it
This is customizable. 

You can also get ZNC.el from the [Marmalade](http://marmalade-repo.org/) ELPA repository. See http://marmalade-repo.org/ for instructions on using Marmalade.
