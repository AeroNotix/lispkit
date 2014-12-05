lispkit
=======

[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/AeroNotix/lispkit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![Imgur](http://i.imgur.com/iWNSIWa.png)


A lisp browser using WebKit


Installation
------------

Ensure you have [Quicklisp](http://quicklisp.org) available locally.

Requirements
------------

* libwebkit2gtk
* gtk2
* SQLite (if you change the cookie backend, default does not require this)
* A CL implementation (tested with SBCL so far)

```shell
cd $QUICKLISP_HOME/local-projects
git clone https://github.com/AeroNotix/cl-xkeysym.git
git clone https://github.com/AeroNotix/lispkit.git
git clone https://github.com/crategus/cl-cffi-gtk.git
git clone https://github.com/joachifm/cl-webkit
sbcl --noinform --quit --eval \
    "(ql:quickload :lispkit)"
```

Status
------

Very early work, but somewhat usable.

Some videos

* Control via SLIME: https://www.youtube.com/watch?v=9GJcct_FyVw
* Keybinds: https://www.youtube.com/watch?v=NxiYnJ_JfRQ
* Navigation: https://www.youtube.com/watch?v=wXQKZX96QDA
* Tabs: https://www.youtube.com/watch?v=3iS9LZxoj6o

Getting Help
------------

Join #lispkit on freenode for help
