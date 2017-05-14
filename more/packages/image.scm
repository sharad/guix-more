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

(define-module (more packages image)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (more packages boost)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public gpick
  (package
    (name "gpick")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/thezbyg/gpick/archive/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mxvxk15xhk2i5vfavjhnkk4j3bnii0gpf8di14rlbpq070hd5rs"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("scons" ,scons)))
    (inputs
     `(("gtk2" ,gtk+-2)
       ("lua" ,lua-5.2)
       ("expat" ,expat)
       ("boost" ,boost)
       ("gettext" ,gnu-gettext)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (substitute* "SConscript"
               (("lua5.2") "lua-5.2"))
             (zero? (system* "scons"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "DESTDIR" (assoc-ref outputs "out"))
             (zero? (system* "scons" "install" (string-append "DESTDIR=" (assoc-ref outputs "out")))))))))
    (home-page "http://www.gpick.org/")
    (synopsis "Color picker")
    (description "Gpick is an advanced color picker and palette editing tool.")
    (license license:bsd-3)))
