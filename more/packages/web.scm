;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix cvs-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages web)
  #:use-module (more packages java))

(define-public java-eclipse-jetty-alpn-api
  (package
    (name "java-eclipse-jetty-alpn-api")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/jetty.alpn.api")
                     (commit "0a2671867f1ad7067bf4070e1b4209f8796d605d")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11f3iq9sdy8si3f3cb9dhd3d9xc0l7cwrsra21jarw01cc9priij"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jetty-alpn.jar"
       #:source-dir "src/main/java"
       #:tests? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-eclipse-jetty-npn-api
  (package
    (name "java-eclipse-jetty-npn-api")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/jetty.project")
                     (commit "cc6196af50edf256c6fa3ead21d726073b08a087")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sn4w6fwbhhzdaizffv04329nd2a7y702a828vvzr6vl8ipxkcv6"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jetty-npn.jar"
       #:source-dir "jetty-npn/src/main/java"
       #:tests? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
