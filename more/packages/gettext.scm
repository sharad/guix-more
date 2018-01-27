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

(define-module (more packages gettext)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (more packages rdf)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wxwidgets))

(define-public poedit
  (package
    (name "poedit")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/vslavik/poedit/releases/"
                                  "download/v" version "-oss/poedit-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1g5ii5wnffjh1xc24c93875r1qqzd024bqizmq400i35dppgxsd9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir=" (assoc-ref %build-inputs "boost") "/lib"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("enchant" ,enchant)
       ("gtk+" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("icu4c" ,icu4c)
       ("lucene++" ,lucene++)
       ("wxwidgets" ,wxwidgets)))
    (home-page "https://poedit.net/")
    (synopsis "Gettext catalog editing tool")
    (description
      "Poedit is a GUI frontend to the GNU gettext utilities and a catalog
editor/source code parser.  It helps with translating applications into
other languages.")
    (license license:expat)))
