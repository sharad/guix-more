;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (more packages boost)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages perl))

(define-public boost-build
  (package
    (name "boost-build")
    (version "2016.03")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/boostorg/build/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sgvl8jisz73ff4vlqjkns4zgjs1yapgw18wmh4dpjp4dhx2ay8y"))))
    (build-system gnu-build-system)
    (inputs
     `(("sh" ,bash)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (begin
               ;; TODO: we can use JAMSHELL here, but not in 'install, so
               ;; I added a dependance on the shell here.
               (substitute* "src/engine/execunix.c"
                 (("/bin/sh") (which "sh")))
               (zero? (system* "./bootstrap.sh")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "./b2" "install"
                             (string-append "--prefix=" (assoc-ref outputs "out")))))))))
    (home-page "http://www.boost.org/build/")
    (synopsis "")
    (description "")
    (license license:boost1.0)))

(define-public boost-fix
  (package
    (inherit boost)
    (name "boost-fix")
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("tcsh" ,tcsh)
       ("which" ,which)))
    (inputs
     `(("zlib" ,zlib)
       ("icu" ,icu4c)))))
