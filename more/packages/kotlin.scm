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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages java)
  #:use-module (more packages maven))

;; Needs maven-core
(define-public kotlin
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
                "1lm1rvj1vf4z8nzpffqcdwcydnlf24ls07z0r0nc4by3hjxzs3sv"))))
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

;; The release page on github is a mess
(define intellij-community-version "0.182.2256")
(define (intellij-community-source version)
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/JetBrains/intellij-community/"
                        "archive/appcode/" (substring version 2) ".tar.gz"))
    (file-name (string-append "intellij-community-" version ".tar.gz"))
    (sha256
     (base32
      "1b9csd0vs6m41q4jhlwl3rlfb596vcnjhch75pcwk8999zrvqf0f"))
    (modules '((guix build utils)))
    (snippet
      `(begin
         (for-each
           (lambda (file)
             (if (file-exists? file)
               (delete-file-recursively file)))
           (append (find-files "." "^lib$" #:directories? #t)
                   (find-files "." "^bin$" #:directories? #t)))
         (for-each delete-file (find-files "." ".*.jar$"))))))

(define-public java-intellij-compiler-instrumentation-util
  (package
    (name "java-intellij-compiler-instrumentation-util")
    (version intellij-community-version)
    (source (intellij-community-source version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "java/compiler/instrumentation-util/src"
       #:jar-name "instrumentation-util.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-imports
           (lambda _
             (substitute* (find-files "java/compiler/instrumentation-util/src" ".*.java")
               (("org.jetbrains.org.objectweb") "org.objectweb")
                ;; As in build/asm/3_api_version.patch
               (("API_VERSION") "ASM6")))))))
    (inputs
     `(("java-asm" ,java-asm)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-compiler-javac2
  (package
    (name "java-intellij-compiler-javac2")
    (version intellij-community-version)
    (source (intellij-community-source version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "java/compiler/javac2/src"
       #:jar-name "javac2.jar"
       ;; No test
       #:tests? #f))
    (inputs
     `(("java-intellij-compiler-instrumentation-util" ,java-intellij-compiler-instrumentation-util)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-platform-forms-rt
  (package
    (name "java-intellij-platform-forms-rt")
    (version intellij-community-version)
    (source (intellij-community-source version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "platform/forms_rt/src"
       #:jar-name "forms_rt.jar"
       ;; No test
       #:tests? #f))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; Newer versions are not free software anymore
;; latest free versions are 1.8.1 and 1.8.0. We require something older for
;; intellij though.
(define-public java-jgoodies-common
  (package
    (name "java-jgoodies-common")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri "http://www.jgoodies.com/download/libraries/common/jgoodies-common-1_8_1.zip")
              (sha256
               (base32
                "1canj4zalrp668c55ji58rk90w005q44lnwzliffsr8mlrgxgaiw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jgoodies-common.jar"
       #:source-dir "."
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'extract-source
           (lambda _
             (invoke "jar" "xf" "jgoodies-common-1.8.1-sources.jar")
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.jgoodies.com")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-jgoodies-forms
  (package
    (name "java-jgoodies-forms")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri "http://www.jgoodies.com/download/libraries/forms/jgoodies-forms-1_8_0.zip")
              (sha256
               (base32
                "1av4w1px1jxmv19mljyicbv657sw5nqhkfx6s7nc5ckzf9ay945h"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jgoodies-forms.jar"
       #:source-dir "."
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'extract-source
           (lambda _
             (invoke "jar" "xf" "jgoodies-forms-1.8.0-sources.jar")
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("java-jgoodies-common" ,java-jgoodies-common)))
    (home-page "http://www.jgoodies.com")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; Requires JGoodies Forms: http://www.jgoodies.com
(define-public java-intellij-compiler-forms-compiler
  (package
    (name "java-intellij-compiler-forms-compiler")
    (version intellij-community-version)
    (source (intellij-community-source version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "java/compiler/forms-compiler/src"
       #:jar-name "forms-compiler.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-imports
           (lambda _
             (substitute* (find-files "java/compiler/forms-compiler/src" ".*.java")
               (("org.jetbrains.org.objectweb") "org.objectweb")
                ;; As in build/asm/3_api_version.patch
               (("API_VERSION") "ASM6"))))
         (add-before 'build 'fix-old-jgoodies
           (lambda _
             (substitute* "java/compiler/forms-compiler/src/com/intellij/uiDesigner/lw/FormLayoutSerializer.java"
               (("new ColumnSpec\\(spec\\)") "ColumnSpec.parse(spec)")))))))
    (inputs
     `(("java-intellij-platform-forms-rt" ,java-intellij-platform-forms-rt)
       ("java-intellij-compiler-instrumentation-util" ,java-intellij-compiler-instrumentation-util)
       ("java-asm" ,java-asm)
       ("java-jdom" ,java-jdom)
       ("java-jgoodies-forms" ,java-jgoodies-forms)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
