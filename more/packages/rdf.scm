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

(define-module (more packages rdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wxwidgets))

(define-public lucene++
  (package
    (name "lucene++")
    (version "3.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/luceneplusplus/"
                                  "LucenePlusPlus/archive/rel_" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "032yb35b381ifm7wb8cy2m3yndklnxyi5cgprjh48jqy641z46bc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; CXX_FLAGS suggested in a closed issue on github:
       ;; https://github.com/luceneplusplus/LucenePlusPlus/issues/100
       (list "-Wno-dev" "-DCMAKE_CXX_FLAGS=-DBOOST_VARIANT_USE_RELAXED_GET_BY_DEFAULT"
             ;; Install in lib64 break rpath
             "-DCMAKE_INSTALL_LIBDIR:PATH=lib")))
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (add-after 'configure 'fix-lib64
       ;    (lambda _
       ;      ;; Install in lib64 break rpath
       ;      (substitute* "Makefile"
       ;        (("lib64") "lib"))
       ;      #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)))
    (home-page "https://github.com/luceneplusplus/LucenePlusPlus")
    (synopsis "Text search engine")
    (description
      "Lucene++ is an up to date C++ port of the popular Java Lucene library,
a high-performance, full-featured text search engine.")
    (license (list license:asl2.0 license:lgpl3)))); either asl or lgpl.
