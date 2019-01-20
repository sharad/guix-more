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

(define-module (more packages geo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public osmconvert
  (package
    (name "osmconvert")
    (version "0")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "http://m.m.i24.cc/osmconvert.c"))
	      (sha256
	       (base32
		"19glwq8w5sl8579zxbpydj56lybs94nrf47f3i2xjwlkmzrlljfv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
	 (delete 'unpack)
	 (delete 'configure)
	 (delete 'install)
	 (replace 'build
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
	     (invoke "gcc" (assoc-ref inputs "source") "-lz" "-o"
		     (string-append (assoc-ref outputs "out") "/bin/osmconvert"))
	     (chmod (string-append (assoc-ref outputs "out") "/bin/osmconvert")
		    #o755)
	     #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:agpl3+)))
