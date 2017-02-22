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

(define-module (gnu packages binary)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public python-pyelftools
  (package
    (name "python-pyelftools")
    (version "0.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eliben/pyelftools/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iw47b20brg0ah86s9a2dn1f70qfmdv20p04q131vmnwa9g066f4"))))
    (build-system python-build-system)
    (home-page "https://github.com/eliben/pyelftools")
    (synopsis "Parsing and analyzing ELF files and DWARF debugging information")
    (description
      "Python library for parsing and analyzing ELF files and DWARF debugging information.")
    (license license:public-domain)))

(define-public python2-pyelftools
  (package-with-python2 python-pyelftools))

(define-public capstone
  (package
    (name "capstone")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aquynh/capstone/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1whl5c8j6vqvz2j6ay2pyszx0jg8d3x8hq66cvgghmjchvsssvax"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://www.capstone-engine.org")
    (synopsis "Disassembler")
    (description
     "Capstone can disassemble machine code for many supported architectures
such as x86, x86_64, arm, arm64, mips, ppc, sparc, sysz and xcore.  It provides
bindings for Python, Java, OCaml and more.")
    (license (list license:bsd-3 license:expat))))

;; This package has a timestamp embedded in
;; lib/python3.5/site-packages/capstone/__pycache__/__iti__.cpython-35.pyc
(define-public python-capstone
  (package
    (inherit capstone)
    (name "python-capstone")
    (propagated-inputs
     `(("capstone" ,capstone)))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-and-fix-setup-py
           (lambda _
             (chdir "bindings/python")
             (substitute* "setup.py" (("data_files=.*") ""))
             (substitute* "capstone/__init__.py"
               (("_lib_path =.*")
                (string-append "_lib_path = '"
                               (assoc-ref %build-inputs "capstone")
                               "/lib'\n")))
             #t)))))))

(define-public python2-capstone
  (package-with-python2 python-capstone))
