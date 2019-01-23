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

(define-module (more packages tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config))

(define-public boringssl
  (package
    (name "boringssl")
    ;; Latest version at the time of packaging
    (version "32e59d2d3264e4e104b355ef73663b8b79ac4093")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://boringssl.googlesource.com/boringssl")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hk90f5svx5zs86syydg3wy4b75m9469byrzj71xsyw6lh6zajjd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f; require ninja?
       #:configure-flags (list "-DBUILD_SHARED_LIBS=1")
       #:validate-runpath? #f; FIXME: this is buggy :/
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-home
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (install-file "ssl/libssl.so" lib)
               (install-file "crypto/libcrypto.so" lib)
               (copy-recursively "../source/include" include))
             #t)))))
    (native-inputs
     `(("go" ,go)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))
