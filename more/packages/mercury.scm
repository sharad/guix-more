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

(define-module (more packages mercury)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib))

(define-public mercury
  (package
    (name "mercury")
    (version "14.01.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.mercurylang.org/release/mercury-srcdist-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "12z8qi3da8q50mcsjsy5bnr4ia6ny5lkxvzy01a3c9blgbgcpxwq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; no test target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-bin-sh
           (lambda _
             (substitute* '(;"library/io.m"
                            "scripts/Mmake.vars.in"
                            "bindist/bindist.Makefile.in"
                            "bindist/bindist.Makefile"
                            "boehm_gc/Makefile.dj"
                            "boehm_gc/Makefile.direct")
                            ;"library/io.c")
               (("/bin/sh") (which "sh")))
             (substitute* '("tools/cvdd" "tools/binary_step" "tools/bootcheck"
                            "tools/binary" "tools/unary" "tools/speedtest"
                            "tools/linear" "scripts/prepare_install_dir.in"
                            "Mmakefile")
               (("/bin/pwd") (which "pwd")))
             (setenv "CONFIG_SHELL" (which "sh"))
             #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (home-page "https://www.mercurylang.org")
    (synopsis "Pure logic programming language")
    (description "Mercury is a pure logic programming language intended for
the creation of large, fast, reliable programs.  The syntax of Mercury is
based on the syntax of Prolog, but semantically the two languages are very
different due to Mercury's purity, its type, mode, determinism and module
systems.")
    (license license:lgpl2.0+)))
