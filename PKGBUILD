# Author: <aaron.l.france@gmail.com>
# Maintainer:  <aaron.l.france@gmail.com>
pkgname=lispkit-browser
pkgver=2
pkgrel=4
pkgdesc="LispKit: A lispy, keyboard-driven browser written in Common Lisp."
arch=('x86_64')
url="https://github.com/AeroNotix/lispkit"
license=('BSD')
provides=('lispkit')
options=('!strip')
source=('http://zerolength.com/bin/lispkit.tar.gz')
md5sums=(':md5sum')
depends=('webkit2gtk')


build() {
  tar xvzf lispkit.tar.gz
}

package() {
  install -Dm775 lispkit "${pkgdir}/usr/bin/lispkit"
}

# vim:set ts=2 sw=2 et:
