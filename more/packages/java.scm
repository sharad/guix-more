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

(define-public java-commons-cli
  (package
    (name "java-commons-cli")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mirrors.ircam.fr/pub/apache/commons/"
                                  "cli/source/commons-cli-" version "-src.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05hgi2z01fqz374y719gl1dxzqvzci5af071zm7vxrjg9vczipm1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-cli-1.4.jar"
       #:tests? #f))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-asm
  (package
    (name "java-asm")
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.forge.ow2.org/asm/asm-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kxvmv5275rnjl7jv0442k3wjnq03ngkb7sghs78avf45pzm4qgr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "asm-5.2.jar"
       #:tests? #f))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; Can only be built with gradle.
(define-public groovy
  (package
    (name "groovy")
    (version "2.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/groovy/archive/GROOVY_"
                                  "2_4_10.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wapzqwpx4bh2fsqpzf3haakjz6wvfjx1vd9a4spavhlrjqk2pbb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "groovy.jar"
       #:tests? #f))
    (native-inputs
     `(("junit" ,java-junit)))
    (inputs
     `(("commons-cli" ,java-commons-cli)
       ("asm" ,java-asm)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; requires jline, commons-cli, javax.servlet, org.fusesource.jansi, org.livetribe,
;;   com.thoughtworks.xstream, org.apache.ivy, bsf, org.apache.ant, junit,
;;   asm, antlr
(define-public groovy-1.8.9
  (package
    (inherit groovy)
    (name "groovy")
    (version "1.8.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/groovy/archive/GROOVY_"
                                  "1_8_9.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16z3jv5yw11wwwzbs6x41g83gqazhngg30ys2kpy7cpfm3rsqi71"))))))
    ;(arguments
    ; `(#:build-target "createJars"))))

;; requires groovy 2.4.7.
(define-public gradle
  (package
    (name "gradle")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gradle/gradle/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0fq30k51mkixg31z3d4fjq3zbnyjml4i530px6n1n947mqk3rgyl"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* _
             (system* "sh" "-x" "gradlew" "prBuild" "-x" "integTest" "--continue"
                      "--stacktrace"))))))
             ;(system* "sh" "-x" "travisci_build.sh"))))))
    (home-page "")
    (synopsis "Build system")
    (description "Build system")
    (license license:asl2.0)))

;; Requires gradle.
(define-public android-anysoft-keyboard
  (package
    (name "android-anysoft-keyboard")
    (version "1.8-r9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/AnySoftKeyboard/"
                                  "AnySoftKeyboard/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mrin9mw1rs23d25v8yx4jprx7j05zir6756sqvk4myxbkcp8mag"))))
    (build-system ant-build-system)
    (home-page "https://anysoftkeyboard.github.io/")
    (synopsis "Alternative on-screen keyboard for multiple languages")
    (description "Alternative on-screen keyboard for multiple languages.")
    (license license:asl2.0)))
