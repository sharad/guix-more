;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (more packages ibus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public libhangul
  (package
    (name "libhangul")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/choehwanjin/libhangul/archive/libhangul-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0c2qpkrsshr55zxlrhibrsvj2j10lvdpf0x2q4y8s2gdb7qixa72"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _ (and (zero? (system* "touch" "ChangeLog"))
                          (zero? (system* "autoreconf" "-fiv"))))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("which" ,which)))
    (home-page "https://github.com/choehwanjin/libhangul")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public ibus-hangul
  (package
    (name "ibus-hangul")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/choehwanjin/ibus-hangul/releases/"
                    "download/" version "/ibus-hangul-" version ".tar.gz"))
              (sha256
               (base32
                "120p9w7za6hi521hz8q235fkl4i3p1qqr8nqm4a3kxr0pcq40bd2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/libexec/" prog)
                    `("PYTHONPATH" ":" prefix
                      (,(getenv "PYTHONPATH")))
                    `("GI_TYPELIB_PATH" ":" prefix
                      (,(getenv "GI_TYPELIB_PATH")
                       ,(string-append out "/lib/girepository-1.0")))))
                '("ibus-engine-hangul" "ibus-setup-hangul"))
               #f))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
    ;   ("gtk+" ,gtk+)
     `(("ibus" ,ibus)
       ("libhangul" ,libhangul)
       ("gobject-introspection" ,gobject-introspection)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Hangul Korean language input method for IBus")
    (description "IBus-Hangul is an engine for the input bus \"IBus\").  It
adds the Hangul Korean language input method to IBus.  Because most graphical
applications allow text input via IBus, installing this package will enable
Korean language input in most graphical applications.")
    (home-page "https://github.com/choehwanjin/ibus-hangul/")
    (license license:gpl2+)))
