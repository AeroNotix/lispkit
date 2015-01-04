lispkit
=======

[![Build Status](https://travis-ci.org/AeroNotix/lispkit.svg)](https://travis-ci.org/AeroNotix/lispkit)

[![Gitter](https://badges.gitter.im/Join
Chat.svg)](https://gitter.im/AeroNotix/lispkit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![Imgur](http://i.imgur.com/iWNSIWa.png)

A lisp web browser using WebKit
-------------------------------

Requirements
------------

* Linux, this is a soft-requirement as I haven't tested at all on Mac.
Windows support is not on the roadmap at all.
  * Probably needs some work on cl-webkit2 to get the libraries found.
  * Probably needs some work on the underlying gtk libraries too.
  * Help wanted!
* libwebkit2gtk
* gtk2
* SQLite (if you change the cookie backend, default does not require this)
* A Common Lisp implementation (tested with SBCL so far)
* Make

How to
------

```shell
make
./lispkit
```

Status
------

Very early work, but somewhat usable.

Some videos:

* Control via SLIME: https://www.youtube.com/watch?v=9GJcct_FyVw
* Keybinds: https://www.youtube.com/watch?v=NxiYnJ_JfRQ
* Navigation: https://www.youtube.com/watch?v=wXQKZX96QDA
* Tabs: https://www.youtube.com/watch?v=3iS9LZxoj6o

Getting Help
------------

Join #lispkit on freenode for help
