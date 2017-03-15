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

(define-module (more packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages java))

(define-public josm
  (package
    (name "josm")
    (version "c86ae64ca82a5bb9dd1972c7023797eb9a2577f5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstreetmap/josm.git")
                    (commit version)))
              (sha256
               (base32
                "07z2q4csq9gdpg4lp1zpvcl5z5sqn0fnqah94ya3sirm6bh4k74j"))
              (file-name (string-append name "-" version))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-compiler
           (lambda* _
             (substitute* "build.xml"
               (("UNKNOWN") "11639")
               ((".*com.google.errorprone.ErrorProneAntCompilerAdapter.*") "")
               (("compiler=\"[^\"]*\" ") ""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib/josm")))
               (mkdir-p bin)
               (mkdir-p lib)
               (copy-file "dist/josm-custom.jar"
                          (string-append lib "/josm.jar"))
               (with-output-to-file (string-append bin "/josm")
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    "java -jar " lib "/josm.jar"))))
               (chmod (string-append bin "/josm") #o755)))))))
    (home-page "https://josm.openstreetmap.de")
    (synopsis "OSM editor")
    (description "OSM editor.")
    (license license:gpl2+)))
