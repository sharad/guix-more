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

(define-module (more packages scala)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java))

;; This package downloads the so-called official version of scala, a pre-built
;; binary by the scala developers.
;; This binary should never be made part of Guix itself, because we have
;; ways to bootstrap it properly. The bootstrap project of scala takes time,
;; so in the meantime... here you are :(
(define-public scala-official
  (package
    (name "scala-official")
    (version "2.12.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.lightbend.com/scala/"
                            version "/scala-" version ".tgz"))
        (sha256
         (base32
          "05ili2959yrshqi44wpmwy0dyfm4kvp6i8mlbnj1xvc5b9649iqs"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (java (assoc-ref %build-inputs "icedtea-8"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (tar (string-append
                                 (assoc-ref %build-inputs "tar")
                                 "/bin/tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (output (assoc-ref %outputs "out"))
                          (bindir (string-append output "/bin")))
                     (mkdir-p output)
                     (setenv "PATH" (string-append (getenv "PATH") ":" gzip "/bin"))
                     (invoke tar "xf" source)
                     (copy-recursively "scala-2.12.6" output)
                     (chdir output)
                     (for-each delete-file (find-files "bin" "bat$"))
                     (substitute* (find-files "bin" ".*")
                       (("^#!.*")
                        (string-append "#!" bash "/bin/bash\n" "JAVA_HOME=" java)))))))
    (inputs
     `(("bash" ,bash)
       ("gzip" ,gzip)
       ("icedtea-8" ,icedtea-8)
       ("tar" ,tar)))
    (home-page "https://scala-lang.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public sjsonnew
  (package
    (name "sjsonnew")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/eed3si9n/sjson-new/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1rv0c50af5kn27x51g650wl2ig94z52fhs0rn8ykahpz4jhg1p7p"))))
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "core/src/main/scala" ".*.scala$"))
             #t)))))
    (build-system ant-build-system)
    (inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public sbt
  (package
    (name "sbt")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/sbt/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15i8fd7zgairaaikscrva8d1klz0w9nh7fc0896x1n8nrs578vmy"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "sbt/src/main/resources" "build/classes")
             #t))
         (replace 'build
           (lambda _
             (define (build-subproject prefix name)
               (let ((build-directory (string-append "build/" name))
                     (jar-name (string-append name ".jar")))
                 (mkdir-p build-directory)
                 (format #t "Building project ~a...~%" name)
                 (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                        "-d" build-directory
                        (find-files (string-append prefix name "/src/main/scala")
                                    ".*.scala$"))
                 (invoke "jar" "cf" jar-name "-C" build-directory)))
             (build-subproject "internal/" "util-collection")
             (build-subproject "" "main-settings")
             (build-subproject "" "sbt")
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("scala" ,scala-official)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))
