;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages intellij)
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
  #:use-module (more packages java))

;; The release page on github is a mess
(define intellij-community-version "182.5262.8")
(define intellij-community-commit "e2a5d9273ec0b3b656c0dad0c9b07e5f85bbd61a")
(define (intellij-community-source commit version)
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/JetBrains/intellij-community")
           (commit commit)))
    (file-name (git-file-name "intellij" version))
    (sha256
     (base32
      "17qzhh2kw6sxwkyj7ng7hrpbcf2rjs2xjbsrg1bgkg90r5kb8sm4"))
    (modules '((guix build utils)))
    (snippet
      `(begin
         (for-each
           (lambda (file)
             (if (file-exists? file)
               (delete-file-recursively file)))
           (append (find-files "." "^lib$" #:directories? #t)
                   (find-files "." "^bin$" #:directories? #t)))
         (delete-file "build.xml")
         (for-each delete-file (find-files "." ".*.jar$"))
         #t))))

(define-public java-intellij-compiler-instrumentation-util
  (package
    (name "java-intellij-compiler-instrumentation-util")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
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
    (source (intellij-community-source intellij-community-commit version))
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
    (source (intellij-community-source intellij-community-commit version))
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

(define-public java-intellij-platform-util-rt
  (package
    (name "java-intellij-platform-util-rt")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "platform/util-rt/src"
       #:jar-name "intellij.platform.util-rt.jar"
       #:jdk ,icedtea-7
       ;; No test
       #:tests? #f))
    (inputs
     `(("java-jetbrains-annotations" ,java-jetbrains-annotations)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-platform-util
  (package
    (name "java-intellij-platform-util")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "platform/util/src"
       #:jar-name "intellij.platform.util.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "platform/util/resources" "build/classes")
             #t))
         (add-before 'build 'remove-apple
           (lambda _
             (delete-file "platform/util/src/com/intellij/util/AppleHiDPIScaledImage.java")
             (delete-file "platform/util/src/com/intellij/util/ui/IsRetina.java")
             #t)))))
    (propagated-inputs
     `(("java-batik" ,java-batik)
       ("java-commons-compress" ,java-commons-compress)
       ("java-imagescalr" ,java-imagescalr)
       ("java-intellij-platform-util-rt" ,java-intellij-platform-util-rt)
       ("java-jakarta-oro" ,java-jakarta-oro)
       ("java-jdom-for-intellij" ,java-jdom-for-intellij)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)
       ("java-log4j-api" ,java-log4j-api)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("java-lz4" ,java-lz4)
       ("java-native-access" ,java-native-access)
       ("java-native-access-platform" ,java-native-access-platform)
       ("java-trove4j-intellij" ,java-trove4j-intellij)
       ("java-w3c-svg" ,java-w3c-svg)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-platform-extensions
  (package
    (name "java-intellij-platform-extensions")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "platform/extensions/src"
       #:jar-name "intellij.platform.extensions.jar"
       ;; No test
       #:tests? #f))
    (propagated-inputs
     `(("java-intellij-platform-util" ,java-intellij-platform-util)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)
       ("java-picocontainer-1" ,java-picocontainer-1)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-platform-core-api
  (package
    (name "java-intellij-platform-core-api")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "platform/core-api/src"
       #:jar-name "intellij.platform.core-api.jar"
       ;; No test
       #:tests? #f))
    (propagated-inputs
     `(("java-automaton" ,java-automaton)
       ("java-intellij-platform-extensions" ,java-intellij-platform-extensions)
       ("java-intellij-platform-resources" ,java-intellij-platform-resources)
       ("java-intellij-platform-util" ,java-intellij-platform-util)
       ("java-intellij-resources" ,java-intellij-resources)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)
       ("java-trove4j-intellij" ,java-trove4j-intellij)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-platform-core-impl
  (package
    (name "java-intellij-platform-core-impl")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "platform/core-impl/src"
       #:jar-name "intellij.platform.core-impl.jar"
       ;; No test
       #:tests? #f))
    (propagated-inputs
     `(("java-guava" ,java-guava)
       ("java-intellij-platform-core-api" ,java-intellij-platform-core-api)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-java-psi-api
  (package
    (name "java-intellij-java-psi-api")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "java/java-psi-api/src"
       #:jar-name "intellij.java.psi-api.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "java/java-psi-api/src/messages"
                               "build/classes/messages")
             #t)))))
    (propagated-inputs
     `(("java-intellij-platform-core-api" ,java-intellij-platform-core-api)
       ("java-intellij-platform-util" ,java-intellij-platform-util)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-java-psi-impl
  (package
    (name "java-intellij-java-psi-impl")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
      ;; TODO: remove these auto-generated files and generate them with
      ;; java-flex from the same-named file in src, with .flex extension
      ;; (_JavaLexer, _JavaDocLexer)
     `(#:source-dir "java/java-psi-impl/src:java/java-psi-impl/gen"
       #:jar-name "intellij.java.psi-impl.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-asm
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* (find-files "java/java-psi-impl/src" ".*\\.java$")
                 (("org.jetbrains.org.objectweb") "org.objectweb")
                 ;; As in build/asm/3_api_version.patch
                 (("API_VERSION") "ASM6")))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "java/java-psi-impl/src/META-INF"
                               "build/classes/META-INF")
             (copy-recursively "java/java-psi-impl/src/messages"
                               "build/classes/intellij/java/resources/en")
             #t)))))
    (propagated-inputs
     `(("java-asm" ,java-asm)
       ("java-intellij-java-psi-api" ,java-intellij-java-psi-api)
       ("java-intellij-platform-core-impl" ,java-intellij-platform-core-impl)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)
       ("java-streamex" ,java-streamex)))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-platform-resources
  (package
    (name "java-intellij-platform-resources")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
      ;; TODO: remove these auto-generated files and generate them with
      ;; java-flex from the same-named file in src, with .flex extension
      ;; (_JavaLexer, _JavaDocLexer)
     `(#:source-dir "platform/platform-resources"
       #:jar-name "intellij.platform.resources.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "platform/platform-resources/src"
                               "build/classes")
             #t)))))
    (propagated-inputs '())
    (native-inputs '())
    (inputs '())
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-resources
  (package
    (name "java-intellij-resources")
    (version intellij-community-version)
    (source (intellij-community-source intellij-community-commit version))
    (build-system ant-build-system)
    (arguments
      ;; TODO: remove these auto-generated files and generate them with
      ;; java-flex from the same-named file in src, with .flex extension
      ;; (_JavaLexer, _JavaDocLexer)
     `(#:source-dir "resources"
       #:jar-name "intellij.resources.jar"
       ;; No test
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "resources/src" "build/classes")
             #t)))))
    (propagated-inputs '())
    (native-inputs '())
    (inputs '())
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
    (source (intellij-community-source intellij-community-commit version))
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
