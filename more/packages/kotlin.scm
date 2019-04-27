;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages kotlin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix cvs-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages batik)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages intellij)
  #:use-module (more packages java))

(define-public kotlin-0
  (package
    (name "kotlin")
    (version "0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/kotlin")
                     (commit "2f47e30a1a12347759dbb8707f5137178de65696")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f60v3swyrkh41c4lhha64njivvsnr7p6yz7i1vjmvs697pjvqg2"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar$"))
                   (mkdir "ideaSDK")
                   (mkdir "dependencies")
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-jars
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (copy-file file (string-append "lib/" (basename file))))
               (apply append
                 (map (lambda (input)
                        (find-files (assoc-ref inputs input) ".*.jar$"))
                   '("java-asm" "java-asm-commons"
                     "java-guava"
                     "java-intellij-java-psi-api"
                     "java-intellij-java-psi-impl"
                     "java-intellij-platform-core-api"
                     "java-intellij-platform-core-impl"
                     "java-intellij-platform-extensions"
                     "java-intellij-platform-util"
                     "java-intellij-platform-util-rt"
                     "java-javax-inject"
                     "java-jsr305"
                     "java-jetbrains-annotations"
                     "java-trove4j-intellij"))))
             #t))
         (add-before 'build 'fix-asm
           (lambda _
             (substitute* (find-files "." ".*.java$")
               (("org.jetbrains.asm4") "org.objectweb.asm"))
             #t)))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-asm-commons" ,java-asm-commons-7)
       ("java-guava" ,java-guava)
       ("java-intellij-java-psi-api" ,java-intellij-java-psi-api)
       ("java-intellij-java-psi-impl" ,java-intellij-java-psi-impl)
       ("java-intellij-platform-core-api" ,java-intellij-platform-core-api)
       ("java-intellij-platform-core-impl" ,java-intellij-platform-core-impl)
       ("java-intellij-platform-extensions" ,java-intellij-platform-extensions)
       ("java-intellij-platform-util" ,java-intellij-platform-util)
       ("java-intellij-platform-util-rt" ,java-intellij-platform-util-rt)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)
       ("java-trove4j-intellij" ,java-trove4j-intellij)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; Needs maven-core
(define-public kotlin-1.2
  (package
    (name "kotlin")
    ;; last version with a build.xml file
    ;; TODO: check if version 1.2.32-1.2.39 exist. 1.2.40 doesn't have build.xml.
    (version "1.2.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JetBrains/kotlin/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lm1rvj1vf4z8nzpffqcdwcydnlf24ls07z0r0nc4by3hjxzs3sv"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar$"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-build.xml
           (lambda _
             (substitute* "build.xml"
               (("org/jetbrains/kotlin/ant/antlib.xml")
                "ant/src/org/jetbrains/kotlin/ant/antlib.xml")))))))
    (native-inputs
     `(("java-intellij-compiler-javac2" ,java-intellij-compiler-javac2)))
    (home-page "https://kotlinlang.org/")
    (synopsis "Programming language targetting the JVM")
    (description "")
    (license license:asl2.0)))

(define-public kotlin
  (package
    (name "kotlin")
    (version "1.3.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/kotlin")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123gfr72p0715n925kivl3khlqzk11qpyv65jwlhcplgfvw8rl73"))))
    (build-system ant-build-system)
    (home-page "https://kotlinlang.org/")
    (synopsis "Programming language targetting the JVM")
    (description "")
    (license license:asl2.0)))
